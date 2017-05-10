module POSIXArgs(getTarballArgs, getLoggerArgs)
 where

import           Control.Monad(foldM)
import qualified Data.ByteString as S
import           Data.List(isPrefixOf)
import           Data.Word(Word16)
import           System.Directory(doesFileExist)
import           System.Environment(getArgs)

getTarballArgs :: IO [(String, S.ByteString)]
getTarballArgs = foldM maybeAddFile [] =<< getArgs
 where
  maybeAddFile acc arg =
    do doAdd <- doesFileExist arg
       if doAdd
          then do contents <- S.readFile arg
                  return ((arg, contents) : acc)
          else return acc

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


