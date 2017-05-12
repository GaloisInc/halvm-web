{-# LANGUAGE OverloadedStrings #-}
module Backend.Hans.Common(HansBackend,initializeHansBackend)
 where

import           Backend(Backend(..))
import           Control.Concurrent(forkIO)
import           Control.Concurrent.Async(Async,async,waitAny)
import           Control.Exception(SomeException, handle, throwIO)
import           Control.Monad(void,mapM_)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import           Data.ByteString.Lazy(ByteString,fromStrict)
import           Data.List(delete)
import           Data.Word(Word16)
import           Hans(NetworkStack, Device, DeviceName, IP4, DeviceConfig,
                      newNetworkStack, defaultDeviceConfig,
                      startDevice, processPackets, defaultConfig)
import           Hans.Device.Types(Device(..))
import           Hans.Dns(HostEntry(..),getHostByName)
import           Hans.IP4.Dhcp.Client(DhcpLease(..), dhcpClient,
                                      defaultDhcpConfig)
import           Hans.IP4.Packet(showIP4,readIP4)
import           Hans.Lens(view)
import           Hans.Socket(TcpListenSocket, TcpSocket,
                             tcpRemoteAddr, tcpRemotePort,
                             newUdpSocket, sendto, sRead, sWrite, sClose,
                             sListen, sAccept, defaultSocketConfig)
import           Syslog(createSyslogger, getLoggerArgs)

type HansBackend = Backend (TcpListenSocket IP4) (TcpSocket IP4)

initializeHansBackend ::
  (NetworkStack -> DeviceName -> DeviceConfig -> IO Device) ->
  [DeviceName] ->
  IO [(String, S.ByteString)] ->
  IO HansBackend
initializeHansBackend addDevice devNames getFiles =
  do ns <- newNetworkStack defaultConfig
     devs <- mapM (\ n -> addDevice ns n defaultDeviceConfig) devNames
     mapM_ startDevice (devs :: [Device])
     void $ forkIO $ handle (\ e -> do putStrLn (show (e :: SomeException))
                                       throwIO e)
                        (processPackets ns)
     (logger, lease) <- waitForGoodLeases ns =<< mapM (getLease ns) devs
     return Backend {
         listen      = hansListen ns (dhcpAddr lease)
       , accept      = hansAccept
       , recv        = hansRecv
       , send        = hansSend
       , close       = hansClose
       , getTarballs = getFiles
       , logMsg      = logger
       }

getLease ::
  NetworkStack ->
  Device ->
  IO (Async (Device, Maybe DhcpLease))
getLease ns dev =
  async $
    do res <- dhcpClient ns defaultDhcpConfig dev
       return (dev, res)

waitForGoodLeases ::
  NetworkStack ->
  [Async (Device, Maybe DhcpLease)] ->
  IO (String -> IO (), DhcpLease)
waitForGoodLeases ns asyncs = go Nothing asyncs
 where
  go Nothing [] =
    fail "Couldn't get a DHCP lease for the server."
  go (Just res) [] =
    return res
  go prev ls =
    do (theFinishedOne, itsAnswer) <- waitAny ls
       case itsAnswer of
         (_, Nothing) ->
           go prev (delete theFinishedOne ls)
         (dev, Just lease) | Nothing <- prev ->
           do let addr = dhcpAddr lease
              logger <- getLogger ns dev addr =<< getLoggerArgs
              logger ("Initialized halvm-web at " ++
                      showIP4 addr " on " ++ S8.unpack (devName dev))
              go (Just (logger, lease)) (delete theFinishedOne ls)
         (dev, Just lease) | Just (logger, _) <- prev ->
           do logger ("Also got DHCP lease for " ++
                      showIP4 (dhcpAddr lease) " on device " ++
                      S8.unpack (devName dev) ++ ".")
              go prev (delete theFinishedOne ls)

getLogger ::
  NetworkStack ->
  Device ->
  IP4 ->
  Maybe (String, String, Word16) ->
  IO (String -> IO ())
getLogger ns dev me margs =
  case margs of
    Nothing -> return (putStrLn . ("LOG: " ++))
    Just (hostname, address, port) ->
      do addr <- getTarget ns address
         udpSock <- newUdpSocket ns defaultSocketConfig (Just dev) me
                    Nothing
         let hostname' = showIP4 me ("/" ++ hostname)
             out       = sendto udpSock addr port . fromStrict
         return (createSyslogger out hostname')

getTarget :: NetworkStack -> String -> IO IP4
getTarget ns str =
  handle (\ e ->
            fail ("Failure in lookup up syslog target: " ++
                  show (e :: SomeException))) $
    case readIP4 str of
      ((x,_):_) -> return x
      _         ->
        do ment <- getHostByName ns (S8.pack str)
           case ment of
             Nothing ->
               fail "Couldn't parse syslog target, and couldn't look it up."
             Just hent ->
               case hostAddresses hent of
                 [] ->
                   fail ("No addresses associated with " ++ str)
                 (addr:_) ->
                   return addr

hansListen :: NetworkStack -> IP4 -> Word16 -> IO (TcpListenSocket IP4)
hansListen ns addr port = sListen ns defaultSocketConfig addr port 5

hansAccept :: TcpListenSocket IP4 ->
              IO (String, Word16, TcpSocket IP4)
hansAccept lsock =
  do res <- sAccept lsock :: IO (TcpSocket IP4)
     let addr = showIP4 (view tcpRemoteAddr res) ""
         port = view tcpRemotePort res
     return (addr, port, res)

hansRecv :: TcpSocket IP4 -> Int -> IO ByteString
hansRecv = sRead

hansSend :: TcpSocket IP4 -> ByteString -> IO Int
hansSend = sWrite

hansClose :: TcpSocket IP4 -> IO ()
hansClose = sClose


