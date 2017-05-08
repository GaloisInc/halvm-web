module Backend.Network(initializeBackend)
 where

import           Backend(Backend(..))
import           Control.Exception(SomeException,handle)
import           Control.Monad(foldM,when,void)
import qualified Data.ByteString as S
import           Data.List(isPrefixOf)
import           Data.Word(Word16)
import           Network.Socket(Socket,AddrInfo(..),AddrInfoFlag(..),
                                Family(..),SocketType(..),SockAddr(..),
                                SocketOption(..),HostAddress, HostAddress6,
                                hostAddressToTuple,hostAddress6ToTuple,
                                getAddrInfo,socket,bind,defaultHints,
                                setSocketOption)
import qualified Network.Socket as Socket(listen,accept,close)
import           Network.Socket.ByteString(sendTo)
import qualified Network.Socket.ByteString.Lazy as Socket(send,recv)
import           Numeric(showHex)
import           Syslog(createSyslogger)
import           System.Directory(doesFileExist)
import           System.Environment(getArgs)

initializeBackend :: IO (Backend Socket Socket)
initializeBackend =
  do logger <- getLogger
     return Backend {
              listen      = bindEverything
            , accept      = runAccept
            , recv        = \ s a -> Socket.recv s (fromIntegral a)
            , send        = \ s b -> fromIntegral `fmap` Socket.send s b
            , close       = Socket.close
            , getTarballs = getTarballArgs
            , logMsg      = logger
            }

bindEverything :: Word16 -> IO Socket
bindEverything port =
  do myAddrs <- getAddrInfo (Just aiHints) Nothing (Just (show port))
     let addrs6 = filter (\ x -> addrFamily x == AF_INET6) myAddrs
         addrs4 = filter (\ x -> addrFamily x == AF_INET)  myAddrs
     -- prefer IPv6 addresses, because they get us everythin
     bindAddress (addrs6 ++ addrs4)
 where
  bindAddress []       = fail ("Couldn't bind port " ++ show port)
  bindAddress (x:rest) =
    handle ((\ _ -> bindAddress rest) :: SomeException -> IO Socket) $
      do sock <- socket AF_INET Stream (addrProtocol x)
         setSocketOption sock ReuseAddr 1
         bind sock (addrAddress x)
         Socket.listen sock 3
         return sock

runAccept :: Socket -> IO (String, Word16, Socket)
runAccept sock =
  do (sock', sockaddr) <- Socket.accept sock
     case sockaddr of
       SockAddrInet  p   ha   -> return (showAddr4 ha, fromIntegral p, sock')
       SockAddrInet6 p _ ha _ -> return (showAddr6 ha, fromIntegral p, sock')
       _                      -> fail "Accepted weird socket type"

getTarballArgs :: IO [(String, S.ByteString)]
getTarballArgs = foldM maybeAddFile [] =<< getArgs
 where
  maybeAddFile acc arg =
    do doAdd <- doesFileExist arg
       if doAdd
          then do contents <- S.readFile arg
                  return ((arg, contents) : acc)
          else return acc

getLogger :: IO (String -> IO ())
getLogger = foldM maybeLogger (putStrLn . ("LOG: " ++)) =<< getArgs
 where
  maybeLogger acc x | "syslog:" `isPrefixOf` x = parseLogger x
                    | otherwise                = return acc

parseLogger :: String -> IO (String -> IO ())
parseLogger x =
  do let (_,        rest0) = span (/= ':') x
         (hostname, rest1) = span (/= ':') (drop 1 rest0)
         (address,  rest2) = span (/= ':') (drop 1 rest1)
     port <- forceRead "port number" (drop 1 rest2)
     ainfos <- getAddrInfo (Just datagramProt) (Just address) (Just "syslog")
     when (null ainfos) $
       fail ("Couldn't look up address of " ++ address)
     let ainfo = head ainfos
     s <- socket (addrFamily ainfo) (addrSocketType ainfo) (addrProtocol ainfo)
     let addr = case addrAddress ainfo of
                  SockAddrInet _ ha -> SockAddrInet port ha
                  SockAddrInet6 _ fi ha sid -> SockAddrInet6 port fi ha sid
                  _ -> error "Invalid target address (not IPv4 or IPv6)"
     return (createSyslogger (\msg -> void (sendTo s msg addr)) hostname)
 where
  datagramProt = defaultHints{ addrSocketType = Datagram }

forceRead :: Read a => String -> String -> IO a
forceRead msg x =
  case reads x of
    [(v, "")] -> return v
    _         -> fail ("Parse error reading syslog " ++ msg)

aiHints :: AddrInfo
aiHints = defaultHints{ addrFlags = [AI_PASSIVE,AI_NUMERICSERV,AI_NUMERICHOST]
                      , addrSocketType = Stream }

showAddr4 :: HostAddress -> String
showAddr4 x = show a ++ "." ++ show b ++ "." ++ show c ++ "." ++ show d
 where (a,b,c,d) = hostAddressToTuple x

showAddr6 :: HostAddress6 -> String
showAddr6 x = showHex a ("::" ++ showHex b ("::" ++ showHex c ("::" ++
              showHex d ("::" ++ showHex e ("::" ++ showHex f ("::" ++
              showHex e ("::" ++ showHex f ("::" ++ showHex g ("::" ++
              showHex h "")))))))))
 where (a,b,c,d,e,f,g,h) = hostAddress6ToTuple x
