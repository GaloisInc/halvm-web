module POSIXArgs(getTarballArgs)
 where

import           Control.Monad(foldM)
import qualified Data.ByteString as S
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


