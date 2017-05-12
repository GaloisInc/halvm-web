module Backend.Hans.HaLVM(initializeBackend)
 where

import           Backend.Hans.Common(HansBackend, initializeHansBackend)
import           Control.Concurrent(threadDelay)
import           Control.Exception(SomeException, handle, throwIO)
import qualified Data.ByteString as S
import           Data.ByteString.Unsafe(unsafePackCStringLen)
import qualified Data.ByteString.Char8 as S8
import           Data.Serialize.Get(runGet, getWord32host)
import           Foreign.Ptr(castPtr,nullPtr)
import           Hans(addDevice)
import           Hans.Device(listDevices)
import           Hypervisor.Console(Console, initXenConsole, writeConsole)
import           Hypervisor.DomainInfo(domainModuleStart, domainModuleLength,
                                       DomainFlags(..), domainFlags)
import           Hypervisor.XenStore(initXenStore)

initializeBackend :: IO HansBackend
initializeBackend =
  do con <- initXenConsole
     handle (handleError con) $
       do xs   <- initXenStore
          nics <- listDevices xs
          initializeHansBackend (addDevice xs) nics (getRamdisk con)

handleError :: Console -> SomeException -> IO a
handleError con e =
  do writeConsole con ("Exception: " ++ show e ++ "\n")
     threadDelay 1000000
     throwIO e

getRamdisk :: Console -> IO [(String, S.ByteString)]
getRamdisk con =
  do start <- domainModuleStart
     case start of
       Left ptr | ptr == nullPtr ->
         do writeConsole con "ERROR: Could not load ramdisk.\n"
            return []
       Left ptr ->
         do len <- domainModuleLength
            res <- unsafePackCStringLen (castPtr ptr, fromIntegral len)
            dflags <- domainFlags
            writeConsole con "Parsing multiboot record.\n"
            if DomainModuleIsMultiboot `elem` dflags
              then return (processMultiboot res)
              else return [("", res)]
       Right _ ->
         do writeConsole con "ERROR: Got PFN from ramdisk; need to fix?\n"
            return []

processMultiboot :: S.ByteString -> [(String, S.ByteString)]
processMultiboot bstr =
  case runGet getHeaders bstr of
    Left  _  -> []
    Right xs -> xs
 where
  getHeaders =
    do modStart   <- fromIntegral `fmap` getWord32host
       modEnd     <- fromIntegral `fmap` getWord32host
       cmdLineOff <- fromIntegral `fmap` getWord32host
       _          <- getWord32host
       let cmdLine = S8.unpack (S.takeWhile (/= 0) (S.drop cmdLineOff bstr))
           modl    = S.take (modEnd - modStart + 1) (S.drop modStart bstr)
       if modStart == 0
         then return []
         else ((cmdLine, modl):) `fmap` getHeaders

