{-# LANGUAGE OverloadedStrings #-}
module Backend.Hans.Common(HansBackend,initializeHansBackend)
 where

import           Backend(Backend(..))
import           Control.Concurrent(forkIO)
import           Control.Exception(SomeException, handle, throwIO)
import           Control.Monad(void)
import qualified Data.ByteString.Char8 as S8
import           Data.ByteString.Lazy(ByteString,fromStrict)
import           Data.Word(Word16)
import           Hans(NetworkStack, Device, DeviceName, IP4,
                      newNetworkStack, addDevice,
                      startDevice, processPackets, defaultConfig,
                      defaultDeviceConfig)
import           Hans.Dns(HostEntry(..),getHostByName)
import           Hans.IP4.Dhcp.Client(DhcpLease(..), dhcpClient,
                                      defaultDhcpConfig)
import           Hans.IP4.Packet(showIP4,readIP4)
import           Hans.Lens(view)
import           Hans.Socket(TcpListenSocket, TcpSocket,
                             tcpRemoteAddr, tcpRemotePort,
                             newUdpSocket, sendto, sRead, sWrite, sClose,
                             sListen, sAccept, defaultSocketConfig)
import           POSIXArgs(getTarballArgs, getLoggerArgs)
import           Syslog(createSyslogger)

type HansBackend = Backend (TcpListenSocket IP4) (TcpSocket IP4)

initializeHansBackend :: DeviceName -> IO HansBackend
initializeHansBackend devName =
  do ns <- newNetworkStack defaultConfig
     dev <- addDevice ns devName defaultDeviceConfig
     startDevice dev
     void $ handle (\ e -> do putStrLn (show (e :: SomeException))
                              throwIO e)
                   (forkIO (processPackets ns))
     mlease <- dhcpClient ns defaultDhcpConfig dev
     case mlease of
       Nothing -> fail "Couldn't get DHCP address"
       Just lease ->
         do let addr = dhcpAddr lease
            logger <- getLogger ns dev addr
            logger ("Connected on device " ++ S8.unpack devName ++
                    " at address " ++ showIP4 addr ".")
            return Backend {
                listen      = hansListen ns addr
              , accept      = hansAccept
              , recv        = hansRecv
              , send        = hansSend
              , close       = hansClose
              , getTarballs = getTarballArgs
              , logMsg      = logger
              }

getLogger :: NetworkStack -> Device -> IP4 -> IO (String -> IO ())
getLogger ns dev me =
  do margs <- getLoggerArgs
     case margs of
       Nothing -> return (putStrLn . ("LOG: " ++))
       Just (hostname, address, port) ->
         do addr <- getTarget ns address
            udpSock <- newUdpSocket ns defaultSocketConfig (Just dev) me Nothing
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


