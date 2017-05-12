module Backend.Hans.POSIX(initializeBackend)
 where

import           Backend.Hans.Common(HansBackend,initializeHansBackend)
import qualified Data.ByteString.Char8 as S8
import           Data.List(isPrefixOf)
import           Hans(addDevice)
import           POSIXArgs(getTarballArgs)
import           System.Environment(getArgs)

initializeBackend :: IO HansBackend
initializeBackend =
  do args <- getArgs
     case filter ("tap" `isPrefixOf`) args of
       [] -> fail "No tap device provided."
       devNames ->
         let devNames' = map S8.pack devNames
         in initializeHansBackend addDevice devNames' getTarballArgs
