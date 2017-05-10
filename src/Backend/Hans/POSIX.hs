module Backend.Hans.POSIX(initializeBackend)
 where

import           Backend.Hans.Common(HansBackend,initializeHansBackend)
import qualified Data.ByteString.Char8 as S8
import           Data.List(find,isPrefixOf)
import           System.Environment(getArgs)

initializeBackend :: IO HansBackend
initializeBackend =
  do args <- getArgs
     case find ("tap" `isPrefixOf`) args of
       Nothing -> fail "No tap device provided."
       Just devName ->
         do res <- initializeHansBackend (S8.pack devName)
            return res
