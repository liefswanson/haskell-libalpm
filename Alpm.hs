{-# LANGUAGE ForeignFunctionInterface #-}

module Alpm
    ( md5
    , sha256
    , alpmVersion) where

import Foreign hiding (unsafePerformIO)
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String

import System.IO.Unsafe

---

foreign import ccall unsafe "alpm.h alpm_version"
        alpm_version :: IO (Ptr CChar)

foreign import ccall unsafe "alpm.h alpm_compute_md5sum"
        alpm_md5 :: CString -> IO (Ptr CChar)

foreign import ccall unsafe "alpm.h alpm_compute_sha256sum"
        alpm_sha256 :: CString -> IO (Ptr CChar)
---

alpmVersion :: String
alpmVersion = unsafePerformIO $ alpm_version >>= peekCString

checksum :: (CString -> IO CString) -> FilePath -> IO String
checksum f fp = withCString fp f >>= peekCString

md5 :: FilePath -> IO String
md5 = checksum alpm_md5

sha256 :: FilePath -> IO String
sha256 = checksum alpm_sha256
