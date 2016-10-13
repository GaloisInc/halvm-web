{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
import           Codec.Archive.Tar(Archive, ArchiveMember(..), unarchive,
                                   regContents, linkTarget)
import           Control.Concurrent(threadDelay, forkIO)
import           Control.Concurrent.Async(Async, async, waitAny)
import           Control.Exception(SomeException, handle)
import           Control.Monad(void, forever, forM_, foldM)
import qualified Data.ByteString as S
import           Data.ByteString(ByteString)
import qualified Data.ByteString.Char8 as C
import           Data.ByteString.Unsafe(unsafePackCStringLen)
import           Data.Char(toLower)
import qualified Data.Map.Strict as Map
import           Data.Serialize.Get(runGet, getWord32host)
import           Data.Text(pack, unpack)
import           Data.Text.Encoding(decodeUtf8)
import           Foreign.Ptr(castPtr, nullPtr)
import           Hans(newNetworkStack, addDevice, startDevice, processPackets,
                       defaultConfig)
import           Hans.Device(Device(..), listDevices, defaultDeviceConfig)
import           Hans.IP4.Packet(IP4, showIP4)
import           Hans.IP4.Dhcp.Client(DhcpLease(..), DhcpConfig(..), dhcpClient,
                                      defaultDhcpConfig)
import           Hans.Addr(NetworkAddr, toAddr, showAddr)
import           Hans.Lens(view)
import           Hans.Socket(TcpListenSocket, sListen, sAccept,
                             tcpRemoteAddr, defaultSocketConfig)
import           Hypervisor.Console(Console, initXenConsole, writeConsole)
import           Hypervisor.DomainInfo(domainModuleStart, domainModuleLength,
                                       DomainFlags(..), domainFlags)
import           Hypervisor.XenStore(initXenStore)
import           Network.HTTP(Request(..), Response(..), ResponseCode)
import           Network.HTTP.Headers(HeaderName(..), retrieveHeaders, mkHeader,
                                      hdrValue)
import           Network.HTTP.Stream(Stream(..), receiveHTTP, respondHTTP)
import           Network.HTTP.Streams()
import           Network.Mime(mimeByExt, defaultMimeMap, defaultMimeType)
import           Network.URI(URI(..))

main :: IO ()
main =
  do con <- initXenConsole
     handle (handleErr con "at top level") $
       do disks   <- getRamdisk con
          archive <- foldM (unarchiveAndMerge con) Map.empty disks
          xs      <- initXenStore
          nics    <- listDevices xs
          ns      <- newNetworkStack defaultConfig
          devs    <- mapM (\ n -> addDevice xs ns n defaultDeviceConfig) nics
          -- make hans go!
          void $ forkIO $ handle (handleErr con "packet processor") $
                            processPackets ns
          forM_ devs startDevice
          -- get our addresses and start our web servers
          writeConsole con ("Found " ++ show (length devs) ++ " devices.\n")
          leases <- mapM (\ d -> async $ handle (handleErr con "dhcp client") $
                           do writeConsole con ("Starting DHCP client for " ++ show (devMac d) ++ "\n")
                              l <- dhcpClient ns (manyRetriesConfig con) d
                              writeConsole con ("Picked up DHCP lease for " ++ show (devMac d) ++ "\n")
                              return (l, d)) devs
          writeConsole con ("Generated " ++ show (length leases) ++ " leases.\n")
          processAsyncs leases $ \ (mlease, dev) ->
            case mlease of
              Nothing ->
                writeConsole con ("Device " ++ show (devMac dev) ++
                                  " failed to get an address.\n")
              Just lease ->
                do let addr = dhcpAddr lease
                   writeConsole con ("Starting listener on " ++ showIP4 addr ""
                                     ++ " (dev " ++ show (devMac dev) ++ ")\n")
                   lsock <- sListen ns defaultSocketConfig addr 80 5
                   void $ forkIO $
                     handle (handleErr con ("on socket for " ++ showIP4 addr "")) $
                       do writeConsole con ("Started on "++showIP4 addr ""++":80\n")
                          forever $ do sock <- sAccept (lsock :: TcpListenSocket IP4)
                                       let r = view tcpRemoteAddr sock
                                       handleClient con (handleReq con archive r) sock
          writeConsole con ("Known keys:\n")
          writeConsole con ("-----------------------------------------------\n")
          forM_ (Map.keys archive) (\ k -> writeConsole con (show k ++ "\n"))
          writeConsole con ("-----------------------------------------------\n")
          forever (threadDelay (15 * 1000 * 1000))

unarchiveAndMerge :: Console -> Archive -> (String, ByteString) -> IO Archive
unarchiveAndMerge con acc (name, bstr) =
  case unarchive bstr of
    Left err ->
      do writeConsole con (show (S.length bstr) ++
                           " byte archive failed to expand: "
                           ++ err ++ "\n")
         return acc
    Right archive ->
      do writeConsole con (show (S.length bstr) ++ " byte archive " ++ name ++
                           " unarchived.\n")
         return (archive `Map.union` acc)

processAsyncs :: [Async a] -> (a -> IO ()) -> IO ()
processAsyncs [] _ = return ()
processAsyncs ls process =
  do (asyncx, x) <- waitAny ls
     process x
     processAsyncs (filter (/= asyncx) ls) process

handleErr :: Console -> String -> SomeException -> IO a
handleErr con place e =
  do writeConsole con ("Caught exception " ++ place ++ ": ")
     writeConsole con (show e ++ "\n")
     threadDelay (10 * 1000 * 1000)
     fail "handleErr"

data Result = Result {
       _resCode :: ResponseCode
     , _resKind :: String
     , _resBody :: ByteString
     }

handleReq :: NetworkAddr addr =>
             Console -> Archive ->
             addr -> Request String ->
             IO Result
handleReq con archive fromAddr req =
  go ("site" ++ uriPath (rqURI req)) (2,0,0) $
    go ("site/404.html") (4,0,4) $
      return (Result (4,0,4) "text/html" builtin404)
 where
  go "site/" code otherwiseDo =
    go "site/index.html" code otherwiseDo
  go key code otherwiseDo = putStrLn ("go " ++ key ++ " " ++ show code ++ "\n") >>
    case Map.lookup key archive of
      Just (RegularFileMember rfile) ->
        do let mimeType = mimeByExt defaultMimeMap defaultMimeType (pack key)
               mimeType' = unpack (decodeUtf8 mimeType)
           writeConsole con ("Request for " ++ key ++ " (" ++ mimeType' ++
                             ") from " ++ showAddr (toAddr fromAddr) "")
           return (Result code mimeType' (regContents rfile))
      Just (LinkMember link) ->
        go (linkTarget link) code otherwiseDo
      Just (SymbolicLinkMember link) ->
        go (linkTarget link) code otherwiseDo
      Nothing ->
        putStrLn ("Nothing found. (" ++ key ++ ")") >> otherwiseDo
      _ ->
        otherwiseDo

builtin404 :: ByteString
builtin404 =
  "<html><head><title>Nope</title></head><body><h1>Page not found!</h1></body>"

getRamdisk :: Console -> IO [(String, ByteString)]
getRamdisk con =
  do start <- domainModuleStart
     case start of
       Left ptr | ptr == nullPtr ->
         do writeConsole con "ERROR: Could not load ramdisk.\n"
            return []
       Left ptr ->
         do len <- domainModuleLength
            res <- unsafePackCStringLen (castPtr ptr, fromIntegral len)
            dflags <- domainFlags
            writeConsole con "Parsing multiboot record.\n"
            if DomainModuleIsMultiboot `elem` dflags
              then return (processMultiboot res)
              else return [("", res)]
       Right _ ->
         do writeConsole con "ERROR: Got PFN from ramdisk; need to fix?\n"
            return []

processMultiboot :: ByteString -> [(String, ByteString)]
processMultiboot bstr =
  case runGet getHeaders bstr of
    Left  _  -> []
    Right xs -> xs
 where
  getHeaders =
    do modStart   <- fromIntegral `fmap` getWord32host
       modEnd     <- fromIntegral `fmap` getWord32host
       cmdLineOff <- fromIntegral `fmap` getWord32host
       _          <- getWord32host
       let cmdLine = C.unpack (S.takeWhile (/= 0) (S.drop cmdLineOff bstr))
           modl    = S.take (modEnd - modStart + 1) (S.drop modStart bstr)
       if modStart == 0
         then return []
         else ((cmdLine, modl):) `fmap` getHeaders

handleClient :: Stream s =>
                Console ->
                (Request String -> IO Result) ->
                s ->
                IO ()
handleClient con buildBody s = void (forkIO (run s))
 where
  run :: Stream s => s -> IO ()
  run sock =
    do writeConsole con "Getting request from client.\n"
       mreq <- receiveHTTP sock
       writeConsole con ("Got request from client: " ++ show mreq ++ "\n")
       case mreq of
         Left  err -> writeConsole con ("Request error: " ++ show err ++ "\n")
         Right req ->
           do Result rspCode contentType body <- buildBody req
              let len = show (S.length body)
                  rspBody = C.unpack body
                  keepAlive = [ mkHeader HdrConnection "keep-alive"
                              | hdr <- retrieveHeaders HdrConnection req
                              , map toLower (hdrValue hdr) == "keep-alive" ]
                  conn | null keepAlive = [ mkHeader HdrConnection "Close" ]
                       | otherwise      = keepAlive
                  rspHeaders = mkHeader HdrContentLength len
                             : mkHeader HdrContentType   contentType
                             : conn
                  rspReason = toReason rspCode
                  resp = Response { .. }
              writeConsole con "Sending response.\n"
              respondHTTP sock resp
              writeConsole con ("Sent response. (" ++ show conn ++ "\n")
              if null keepAlive then close sock else run sock

toReason :: ResponseCode -> String
toReason (2,0,0) = "OK"
toReason (4,0,4) = "Not Found"
toReason (_,_,_) = error "No reason found!"

manyRetriesConfig :: Console -> DhcpConfig
manyRetriesConfig _ = defaultDhcpConfig{ dcRetries = 50000 }
