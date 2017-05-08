{-# LANGUAGE OverloadedStrings #-}
module HTTP(handleConnection)
 where

import           Backend(Backend(..),handleErr,sendAll)
import           Control.Applicative
import           Control.Monad(void)
import           Data.Attoparsec.ByteString as P
import           Data.Attoparsec.ByteString.Char8(char8, endOfLine, isDigit_w8,
                                                  isEndOfLine,isHorizontalSpace)
import           Data.ByteString (ByteString)
import           Data.ByteString.Builder(Builder,string8,intDec,
                                         byteString, toLazyByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import           Data.Monoid((<>),mempty,mconcat)
import           Data.Time.Clock(UTCTime,getCurrentTime)
import           Data.Time.Format(formatTime,defaultTimeLocale)
import           Data.Version(showVersion)
import           Data.Word (Word8,Word16)
import           Paths_halvm_web(version)

handleConnection :: Backend ls s -> s -> String -> Word16 ->
                    (FilePath -> IO (Int,String,Maybe UTCTime,S.ByteString)) ->
                    IO ()
handleConnection backend sock host port handleReq =
  void $ handleErr backend (host ++ ":" ++ show port) $
    do (req, _) <- get (P.parse request S.empty) []
       now      <- getCurrentTime
       case requestMethod req of
         "GET" -> do (code,mimeType,lmod,body) <-
                        handleReq (S8.unpack (requestUri req))
                     let bstr = buildResponse req now code mimeType []
                                              lmod body
                     sendAll backend sock bstr
                     close backend sock
         _     -> do let bstr = buildResponse req now 405 "text/html"
                                              [allowGET] Nothing ""
                     sendAll backend sock bstr
                     close backend sock
 where
  get :: P.Result (Request, [Header]) -> [S.ByteString] ->
         IO (Request, [Header])
  get (Fail _ _ msg) _        = fail ("Parse error: " ++ msg)
  get (Partial f)    []       = do buf <- recv backend sock 512
                                   if L.null buf
                                     then get (f S.empty) []
                                     else get (Partial f) (L.toChunks buf)
  get (Partial f)    (x:rest) = get (f x) rest
  get (Done _ res)   _        = return res

allowGET :: String
allowGET = "Allow: GET"

-- How to build HTTP responses; this is pretty weak

buildResponse :: Request ->
                 UTCTime -> Int -> String -> [String] ->
                 Maybe UTCTime -> S.ByteString ->
                 L.ByteString
buildResponse req now respCode mimeType outHdrs lmod bod =
  toLazyByteString $ hdrLine
                  <> dateLine
                  <> meLine
                  <> lastModified
                  <> contentInfo
                  <> pleaseGoAway
                  <> mconcat (map string8 outHdrs)
                  <> eol
                  <> byteString bod
 where
  hdrLine = string8 "HTTP/" <> byteString (requestVersion req) <>
            string8 " " <> intDec respCode <> " " <>
            responseCodeBuilder respCode <> eol
  dateLine = string8 "Date: " <> dateBuilder now <> eol
  meLine = string8 "Server: halvm-web/" <> string8 (showVersion version) <> eol
  lastModified = maybe mempty toLastModified lmod
  contentInfo | S.null bod = mempty
              | otherwise  = string8 "Content-Length: " <>
                             intDec (S.length bod) <> eol <>
                             string8 "Content-Type: " <>
                             string8 mimeType <> eol
  pleaseGoAway = string8 "Connection: Closed" <> eol

eol :: Builder
eol = string8 "\r\n"

toLastModified :: UTCTime -> Builder
toLastModified date = string8 "Last-Modified: " <> dateBuilder date

dateBuilder :: UTCTime -> Builder
dateBuilder =
  string8 . formatTime defaultTimeLocale "%a, %d %b %0Y %H:%M:%S GMT"

responseCodeBuilder :: Int -> Builder
responseCodeBuilder 100 = string8 "Continue"
responseCodeBuilder 101 = string8 "Switching Protocols"
responseCodeBuilder 200 = string8 "OK"
responseCodeBuilder 201 = string8 "Created"
responseCodeBuilder 202 = string8 "Accepted"
responseCodeBuilder 203 = string8 "Non-Auhtoritative Information"
responseCodeBuilder 204 = string8 "No Content"
responseCodeBuilder 205 = string8 "Reset Content"
responseCodeBuilder 206 = string8 "Partial Content"
responseCodeBuilder 300 = string8 "Multiple Choices"
responseCodeBuilder 301 = string8 "Moved Permanently"
responseCodeBuilder 302 = string8 "Found"
responseCodeBuilder 303 = string8 "See Other"
responseCodeBuilder 304 = string8 "Not Modified"
responseCodeBuilder 305 = string8 "Use Proxy"
responseCodeBuilder 307 = string8 "Temporary Redirect"
responseCodeBuilder 400 = string8 "Bad Request"
responseCodeBuilder 401 = string8 "Unauthorized"
responseCodeBuilder 402 = string8 "Payment Required"
responseCodeBuilder 403 = string8 "Forbidden"
responseCodeBuilder 404 = string8 "Not Found"
responseCodeBuilder 405 = string8 "Method Not Allowed"
responseCodeBuilder 406 = string8 "Not Acceptable"
responseCodeBuilder 407 = string8 "Proxy Authentication Required"
responseCodeBuilder 408 = string8 "Request Timeout"
responseCodeBuilder 409 = string8 "Conflict"
responseCodeBuilder 410 = string8 "Gone"
responseCodeBuilder 411 = string8 "Length Required"
responseCodeBuilder 412 = string8 "Precondition Failed"
responseCodeBuilder 413 = string8 "Request Entity Too Large"
responseCodeBuilder 414 = string8 "Request-URI Too Long"
responseCodeBuilder 415 = string8 "Unsupported Media Type"
responseCodeBuilder 416 = string8 "Requested Range Not Satisfiable"
responseCodeBuilder 417 = string8 "Expectation Failed"
responseCodeBuilder 500 = string8 "Internal Server Error"
responseCodeBuilder 501 = string8 "Not Implemented"
responseCodeBuilder 502 = string8 "Bad Gateway"
responseCodeBuilder 503 = string8 "Service Unavailable"
responseCodeBuilder 504 = string8 "Gateway Timeout"
responseCodeBuilder 505 = string8 "HTTP Version Not Supported"
responseCodeBuilder x   = string8 "Unknown response code " <> intDec x

-- The below is shamelessly taken from the attoparsec examples directory

isToken :: Word8 -> Bool
isToken w = w <= 127 && notInClass "\0-\31()<>@,;:\\\"/[]?={} \t" w

skipSpaces :: Parser ()
skipSpaces = satisfy isHorizontalSpace *> skipWhile isHorizontalSpace

data Request = Request {
      requestMethod  :: ByteString
    , requestUri     :: ByteString
    , requestVersion :: ByteString
    } deriving (Eq, Ord, Show)

httpVersion :: Parser ByteString
httpVersion = "HTTP/" *> P.takeWhile (\c -> isDigit_w8 c || c == 46)

requestLine :: Parser Request
requestLine = Request <$> (takeWhile1 isToken <* char8 ' ')
                      <*> (takeWhile1 (/=32) <* char8 ' ')
                      <*> (httpVersion <* endOfLine)

data Header = Header {
      _headerName  :: ByteString
    , _headerValue :: [ByteString]
 } deriving (Eq, Ord, Show)

messageHeader :: Parser Header
messageHeader = Header
  <$> (P.takeWhile isToken <* char8 ':' <* skipWhile isHorizontalSpace)
  <*> ((:) <$> (takeTill isEndOfLine <* endOfLine)
           <*> (many $ skipSpaces *> takeTill isEndOfLine <* endOfLine))

request :: Parser (Request, [Header])
request = (,) <$> requestLine <*> many messageHeader <* endOfLine
