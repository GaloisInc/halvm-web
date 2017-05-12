{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
import           Codec.Archive.Tar(Archive, ArchiveMember(..), unarchive,
                                   regContents, regModificationTime, linkTarget)
import           Control.Monad(forever, foldM)
import qualified Data.ByteString as S
import           Data.ByteString(ByteString)
import           Data.List(isPrefixOf)
import qualified Data.Map.Strict as Map
import           Data.Text(pack, unpack)
import           Data.Text.Encoding(decodeUtf8)
import           Data.Time.Clock(UTCTime,getCurrentTime)
import           Data.Time.Format(formatTime,defaultTimeLocale)
import           Data.Word(Word16)
import           Network.Mime(mimeByExt, defaultMimeMap, defaultMimeType)
import           System.Environment(getArgs)
import           System.FilePath((</>))

import           Backend(Backend(..),handleErr)
#if defined(HaLVM_HOST_OS)
import           Backend.Hans.HaLVM(initializeBackend)
#elif defined(FORCE_HANS)
import           Backend.Hans.POSIX(initializeBackend)
#else
import           Backend.Network(initializeBackend)
#endif

import           HTTP(handleConnection)

main :: IO ()
main =
  do backend  <- initializeBackend
     tarballs <- getTarballs backend
     archive  <- foldM (unarchiveAndMerge backend) Map.empty tarballs
     port     <- getPort
     lsock    <- listen backend port
     logMsg backend ("Ready to go with " ++ show (Map.size archive) ++
                     " files.")
     forever $
       handleErr backend "accept loop" $
         do (host, theirport, sock) <- accept backend lsock
            handleConnection backend sock host theirport $ \ req ->
              do let req' = "site" ++ req
                 res@(rsp,_,_,body) <-  handleRequest backend req' archive
                 logRequest backend host req rsp (S.length body)
                 return res

getPort :: IO Word16
getPort =
  do args <- getArgs
     case filter ("port:" `isPrefixOf`) args of
       [] -> return 80
       xs ->
         case reads (drop 5 (last xs)) of
           (x,_):_ -> return x
           _ -> fail "Couldn't parse port number."

logRequest :: Backend ls s -> String -> FilePath -> Int -> Int -> IO ()
logRequest backend host path statusCode bodySize =
  do now <- getCurrentTime
     let logTime = formatTime defaultTimeLocale logTimeFormat now
     logMsg backend (host ++ " - - " ++ logTime ++ " \"GET " ++ path ++ "\" " ++
                     show statusCode ++ " " ++ show bodySize)
 where
  logTimeFormat = "[%d/%b/%0Y:%H:%M:%S -0000]"

handleRequest :: Backend ls s -> FilePath -> Archive ->
                 IO (Int, String, Maybe UTCTime, S.ByteString)
handleRequest backend fpath archive =
  case Map.lookup fpath archive of
    Just (RegularFileMember rfile) ->
      do let mimeType  = mimeByExt defaultMimeMap defaultMimeType (pack fpath)
             mimeType' = unpack (decodeUtf8 mimeType)
             modt      = regModificationTime rfile
             code | fpath == "site/404.hml" = 404
                  | otherwise               = 200
         return (code, mimeType', Just modt, regContents rfile)
    Just (LinkMember link) ->
      handleRequest backend (linkTarget link) archive
    Just (SymbolicLinkMember link) ->
      handleRequest backend (linkTarget link) archive
    Just (DirectoryMember _) ->
      handleRequest backend (fpath </> "index.html") archive
    Just _ ->
      handleRequest backend "site/404.html" archive
    Nothing | fpath == "site/404.html" ->
      return (404, "text/html", Nothing, builtin404)
    Nothing | fpath == "site/" ->
      handleRequest backend "site/index.html" archive
    Nothing ->
      handleRequest backend "site/404.html" archive

builtin404 :: S.ByteString
builtin404 =
  "<html><head><title>Nope</title></head><body><h1>Page not found!</h1></body>"

unarchiveAndMerge :: Backend ls s ->
                     Archive -> (String, ByteString) ->
                     IO Archive
unarchiveAndMerge backend acc (name, bstr) =
  case unarchive bstr of
    Left err ->
      do logMsg backend (show (S.length bstr) ++ " byte archive " ++ name ++
                         " failed to expand: " ++ err)
         return acc
    Right archive ->
      do logMsg backend (show (S.length bstr) ++ " byte archive " ++ name ++
                         " unarchived.")
         return (archive `Map.union` acc)
