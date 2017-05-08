module Syslog(createSyslogger) where

import           Backend
import           Data.ByteString(ByteString)
import qualified Data.ByteString.Char8 as S8
import           Data.Time.Clock(UTCTime,getCurrentTime)
import           Data.Time.Format(formatTime,defaultTimeLocale)

createSyslogger :: (ByteString -> IO ()) -> String -> String -> IO ()
createSyslogger sender hostname msg =
  do msg' <- buildSyslogMsg hostname msg
     sender (S8.pack msg')

buildSyslogMsg :: String -> String -> IO String
buildSyslogMsg hostname msg =
  do now <- getCurrentTime
     let timestamp = formatTime defaultTimeLocale syslogFormat now
     return ("<003>1 "++timestamp++" "++hostname++" halvm-web 23 - "++msg)
 where
  syslogFormat = "%0Y-%m-%dT%H:%M:%S%QZ"
