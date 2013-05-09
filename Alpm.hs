{-# LANGUAGE ForeignFunctionInterface #-}

module Alpm
    ( md5
    , sha256
    , alpmVersion) where

import Foreign hiding (unsafePerformIO, void)
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String

import Control.Monad (void)
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
        c_alpm_initialize :: CString -> CString -> Ptr AlpmError -> IO AlpmHandle

foreign import ccall unsafe "alpm.h alpm_release"
        c_alpm_release :: AlpmHandle -> IO CInt

foreign import ccall unsafe "alpm.h alpm_get_localdb"
        c_alpm_localdb :: AlpmHandle -> IO Database

foreign import ccall unsafe "alpm.h alpm_db_get_name"
        c_alpm_db_name :: Database -> IO CString

foreign import ccall unsafe "alpm.h alpm_option_get_arch"
        c_alpm_arch :: AlpmHandle -> IO CString

foreign import ccall unsafe "alpm.h alpm_option_get_lockfile"
        c_alpm_lockfile :: AlpmHandle -> IO CString

foreign import ccall unsafe "alpm.h alpm_errno"
        c_alpm_errno :: AlpmHandle -> AlpmError

foreign import ccall unsafe "alpm.h alpm_strerror"
        c_alpm_error :: AlpmError -> IO CString

---

data DatabaseStruct
data AlpmHandleStruct
type AlpmHandle = Ptr AlpmHandleStruct
type Database   = Ptr DatabaseStruct
type AlpmError  = CInt

rootPath :: FilePath
rootPath = "/"

databasePath :: FilePath
databasePath = "/var/lib/pacman/"

maybeNull :: Storable a => Ptr a -> IO (Maybe a)
maybeNull = maybeNullBy peek

maybeNullBy :: (Ptr a -> IO b) -> Ptr a -> IO (Maybe b)
maybeNullBy f ptr = if ptr == nullPtr then return Nothing else Just `fmap` f ptr

maybeNullString :: CString -> IO (Maybe String)
maybeNullString = maybeNullBy peekCString

initialize :: IO AlpmHandle
initialize = alloca $ \errorPtr -> withCString rootPath $ \root ->
             withCString databasePath $ \database ->
                 c_alpm_initialize root database errorPtr

release :: AlpmHandle -> IO ()
release = void . c_alpm_release

localDatabase :: AlpmHandle -> IO Database
localDatabase = c_alpm_localdb

databaseName :: Database -> Maybe String
databaseName d = unsafePerformIO $ c_alpm_db_name d >>= maybeNullString

-- Does this need to be set first?
-- It always seems to be returning a null pointer.
architecture :: AlpmHandle -> Maybe String
architecture h = unsafePerformIO $ c_alpm_arch h >>= maybeNullString

-- | Read-only.
lockFile :: AlpmHandle -> FilePath
lockFile h = unsafePerformIO $ c_alpm_lockfile h >>= peekCString

alpmError :: AlpmHandle -> String
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
