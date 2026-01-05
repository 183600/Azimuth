{-# LANGUAGE CPP #-}
module Paths_azimuth (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0] []

prefix, bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

prefix = "/usr/local"
bindir = "/usr/local/bin"
libdir = "/usr/local/lib"
dynlibdir = "/usr/local/lib"
datadir = "/usr/local/share"
libexecdir = "/usr/local/libexec"
sysconfdir = "/usr/local/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "azimuth_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "azimuth_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "azimuth_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "azimuth_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "azimuth_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "azimuth_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)