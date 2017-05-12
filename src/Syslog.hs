module Syslog(createSyslogger, getLoggerArgs) where

import           Control.Monad(foldM)
import           Data.ByteString(ByteString)
import qualified Data.ByteString.Char8 as S8
import           Data.List(isPrefixOf)
import           Data.Word(Word16)
import           Data.Time.Clock(getCurrentTime)
import           Data.Time.Format(formatTime,defaultTimeLocale)
import           System.Environment(getArgs)

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

getLoggerArgs :: IO (Maybe (String, String, Word16))
getLoggerArgs = foldM maybeLogger Nothing =<< getArgs
 where
  maybeLogger acc x | "syslog:" `isPrefixOf` x = Just `fmap` parseLogger x
                    | otherwise                = return acc

parseLogger :: String -> IO (String, String, Word16)
parseLogger x =
  do let (_,        rest0) = span (/= ':') x
         (hostname, rest1) = span (/= ':') (drop 1 rest0)
         (address,  rest2) = span (/= ':') (drop 1 rest1)
     port <- forceRead "port number" (drop 1 rest2)
     return (hostname, address, port)

forceRead :: Read a => String -> String -> IO a
forceRead msg x =
  case reads x of
    [(v, "")] -> return v
    _         -> fail ("Parse error reading syslog " ++ msg)


