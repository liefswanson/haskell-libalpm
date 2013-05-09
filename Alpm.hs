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

foreign import ccall unsafe "alpm.h alpm_option_get_arch"
        c_alpm_arch :: AlpmHandle -> IO CString

foreign import ccall unsafe "alpm.h alpm_option_get_lockfile"
        c_alpm_lockfile :: AlpmHandle -> IO CString

foreign import ccall unsafe "alpm.h alpm_errno"
        c_alpm_errno :: AlpmHandle -> AlpmError

foreign import ccall unsafe "alpm.h alpm_strerror"
        c_alpm_error :: AlpmError -> IO CString

---------------------
-- Database functions
---------------------
foreign import ccall unsafe "alpm.h alpm_get_localdb"
        c_alpm_localdb :: AlpmHandle -> IO Database

foreign import ccall unsafe "alpm.h alpm_db_get_name"
        c_alpm_db_name :: Database -> IO CString

foreign import ccall unsafe "alpm.h alpm_db_get_pkg"
        c_alpm_get_pkg :: Database -> CString -> IO AlpmPkg

localDatabase :: AlpmHandle -> IO Database
localDatabase = c_alpm_localdb

databaseName :: Database -> Maybe String
databaseName d = unsafePerformIO $ c_alpm_db_name d >>= maybeNullString

alpmPkg :: Database -> String -> IO (Maybe AlpmPkg)
alpmPkg d p = withCString p $ \p' -> maybeNull `fmap` c_alpm_get_pkg d p'

--------------------
-- AlpmPkg functions
--------------------
foreign import ccall unsafe "alpm.h alpm_pkg_get_version"
        c_alpm_pkg_version :: AlpmPkg -> IO CString

foreign import ccall unsafe "alpm.h alpm_pkg_get_isize"
        c_alpm_isize :: AlpmPkg -> CInt

alpmPkgVersion :: AlpmPkg -> String
alpmPkgVersion p = unsafePerformIO $ c_alpm_pkg_version p >>= peekCString

-- | Result is in bytes.
installedSize :: AlpmPkg -> Int
installedSize = fromIntegral . c_alpm_isize

---

test n = do
  p <- initialize >>= localDatabase >>= flip alpmPkg n
  case p of
    Just p' -> return $ installedSize p'
    Nothing -> return 0

data AlpmHandleStruct
data DatabaseStruct
data AlpmPkgStruct
type AlpmHandle = Ptr AlpmHandleStruct
type Database   = Ptr DatabaseStruct
type AlpmPkg    = Ptr AlpmPkgStruct
type AlpmError  = CInt

rootPath :: FilePath
rootPath = "/"

databasePath :: FilePath
databasePath = "/var/lib/pacman/"

maybeNull :: Ptr a -> Maybe (Ptr a)
maybeNull p = if p == nullPtr then Nothing else Just p

maybeNullPeekBy :: (Ptr a -> IO b) -> Ptr a -> IO (Maybe b)
maybeNullPeekBy f p = if p == nullPtr then return Nothing else Just `fmap` f p

maybeNullString :: CString -> IO (Maybe String)
maybeNullString = maybeNullPeekBy peekCString

initialize :: IO AlpmHandle
initialize = alloca $ \errorPtr -> withCString rootPath $ \root ->
             withCString databasePath $ \database ->
                 c_alpm_initialize root database errorPtr

release :: AlpmHandle -> IO ()
release = void . c_alpm_release

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
