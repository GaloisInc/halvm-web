module Backend(Backend(..), handleErr, sendAll)
 where

import           Control.Exception(SomeException, handle)
import qualified Data.ByteString      as S
import qualified Data.ByteString.Lazy as L
import           Data.Word(Word16)

data Backend lsock sock = Backend
  { listen      :: Word16 ->                 IO lsock
  , accept      :: lsock  ->                 IO (String, Word16, sock)
  , recv        :: sock   -> Int          -> IO L.ByteString
  , send        :: sock   -> L.ByteString -> IO Int
  , close       :: sock   ->                 IO ()
  --
  , getTarballs :: IO [(String, S.ByteString)]
  , logMsg      :: String -> IO ()
  }

handleErr :: Backend lsock sock -> String -> IO () -> IO ()
handleErr backend place = handle oops
 where
  oops :: SomeException -> IO () 
  oops se =
    do let msg = "Caught exception in " ++ place ++ ": " ++ show se
       logMsg backend msg

sendAll :: Backend lsock sock -> sock -> L.ByteString -> IO ()
sendAll backend sock lbstr
  | L.null lbstr = return ()
  | otherwise    = do amt <- send backend sock lbstr
                      sendAll backend sock (L.drop (fromIntegral amt) lbstr)
