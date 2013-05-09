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
        c_alpm_version :: IO CString

foreign import ccall unsafe "alpm.h alpm_compute_md5sum"
        c_alpm_md5 :: CString -> IO CString

foreign import ccall unsafe "alpm.h alpm_compute_sha256sum"
        c_alpm_sha256 :: CString -> IO CString

foreign import ccall unsafe "alpm.h alpm_pkg_vercmp"
        c_alpm_vercmp :: CString -> CString -> IO CInt

foreign import ccall unsafe "alpm.h alpm_initialize"
        c_alpm_initialize :: CString -> CString -> Ptr AlpmError -> IO (Ptr AlpmHandle)

foreign import ccall unsafe "alpm.h alpm_errno"
        c_alpm_errno :: Ptr AlpmHandle -> AlpmError

foreign import ccall unsafe "alpm.h alpm_strerror"
        c_alpm_error :: AlpmError -> IO CString

---

data AlpmHandle
type AlpmError = CInt

rootPath :: FilePath
rootPath = "/"

databasePath :: FilePath
databasePath = "/var/lib/pacman/"

initialize :: IO (Ptr AlpmHandle)
initialize = alloca $ \errorPtr -> withCString rootPath $ \root ->
             withCString databasePath $ \database ->
                 c_alpm_initialize root database errorPtr

alpmError :: Ptr AlpmHandle -> String
alpmError h = unsafePerformIO $ c_alpm_error eno >>= peekCString
    where eno = c_alpm_errno h

alpmVersion :: String
alpmVersion = unsafePerformIO $ c_alpm_version >>= peekCString

checksum :: (CString -> IO CString) -> FilePath -> IO String
checksum f fp = withCString fp f >>= peekCString

md5 :: FilePath -> IO String
md5 = checksum c_alpm_md5

sha256 :: FilePath -> IO String
sha256 = checksum c_alpm_sha256

verCmp :: String -> String -> Ordering
verCmp v1 v2 = case result of 0 -> EQ; 1 -> GT; _ -> LT
    where result = fromIntegral $ unsafePerformIO $
                   withCString v1 $ \s1 ->
                       withCString v2 $ \s2 ->
                           c_alpm_vercmp s1 s2
