{-# LANGUAGE MultiWayIf #-}
module Network.HTTP.Streams(
         TLSStream
       , buildTLSStream
       )
 where

import           Control.Concurrent(threadDelay)
import           Control.Concurrent.MVar(MVar, newMVar, takeMVar, putMVar)
import           Data.ByteString(ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as SC
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BSC
import           Hans.Network(Network)
import           Hans.Socket(TcpSocket, sClose, sRead, sCanWrite, sWrite)
import           Network.HTTP.Stream(Stream(..), ConnError(..))
import           Network.TLS(Context, sendData, recvData, contextClose)

instance Network addr => Stream (TcpSocket addr) where
  readLine s = loop ""
   where loop acc =
           do bstr <- sRead s 1
              if | BS.null bstr       -> return (Left ErrorClosed)
                 | BS.head bstr == 10 -> return (Right (acc ++ "\n"))
                 | otherwise          -> loop (acc ++ BSC.unpack bstr)
  readBlock s x = loop (fromIntegral x) BS.empty
    where loop 0 acc = return (Right (BSC.unpack acc))
          loop y acc =
            do bstr <- sRead s (fromIntegral y)
               if | BS.length bstr == y -> loop 0 (acc `BS.append` bstr)
                  | BS.length bstr == 0 -> return (Left ErrorClosed)
                  | otherwise           -> loop (y - BS.length bstr)
                                                (acc `BS.append` bstr)
  writeBlock s str = loop (BSC.pack str)
    where loop x | BS.null x = return (Right ())
                 | otherwise =
                    do amt <- sWrite s x
                       if amt == 0
                         then wait x
                         else loop (BS.drop (fromIntegral amt) x)
          wait x = do goodToGo <- sCanWrite s
                      if goodToGo
                        then loop x
                        else threadDelay 500000 >> wait x
  close s = sClose s
  closeOnEnd _ _ = return ()

data TLSStream = TLSStream (MVar ByteString) Context

instance Stream TLSStream where
  readLine c = loop ""
   where loop acc =
           do Right bstr <- readBlock c 1
              if | null bstr    -> return (Left ErrorClosed)
                 | bstr == "\n" -> return (Right (acc ++ "\n"))
                 | otherwise    -> loop (acc ++ bstr)
  readBlock c@(TLSStream mv _) x = loop x ""
   where loop amt acc =
           do buf <- takeMVar mv
              if S.length buf >= amt
                then do let (ret, buf') = S.splitAt amt buf
                        putMVar mv buf'
                        return (Right (acc ++ SC.unpack ret))
                else do putMVar mv S.empty
                        nextPacket c
                        loop (amt - S.length buf) (acc ++ SC.unpack buf)
  writeBlock (TLSStream _ c) str = do sendData c (BSC.pack str)
                                      return (Right ())
  close (TLSStream _ c) = contextClose c
  closeOnEnd _ _ = return ()

buildTLSStream :: Context -> IO TLSStream
buildTLSStream c =
  do mvar <- newMVar S.empty
     return (TLSStream mvar c)

nextPacket :: TLSStream -> IO ()
nextPacket (TLSStream mv c) =
  do _ <- takeMVar mv
     p <- recvData c
     putMVar mv p


