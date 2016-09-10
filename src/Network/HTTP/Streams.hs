{-# LANGUAGE MultiWayIf #-}
module Network.HTTP.Streams(
       )
 where

import           Control.Concurrent(threadDelay)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BSC
import           Hans.Network(Network)
import           Hans.Socket(TcpSocket, sClose, sRead, sCanWrite, sWrite)
import           Network.HTTP.Stream(Stream(..), ConnError(..))

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

