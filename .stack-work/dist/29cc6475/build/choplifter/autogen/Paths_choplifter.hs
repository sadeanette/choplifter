{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_choplifter (
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
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\W530\\funktio-projektit\\choplifter\\.stack-work\\install\\8e21f95a\\bin"
libdir     = "C:\\Users\\W530\\funktio-projektit\\choplifter\\.stack-work\\install\\8e21f95a\\lib\\x86_64-windows-ghc-8.8.4\\choplifter-0.1.0.0-JKWEOsPVeDV5xrcsaZeGQW-choplifter"
dynlibdir  = "C:\\Users\\W530\\funktio-projektit\\choplifter\\.stack-work\\install\\8e21f95a\\lib\\x86_64-windows-ghc-8.8.4"
datadir    = "C:\\Users\\W530\\funktio-projektit\\choplifter\\.stack-work\\install\\8e21f95a\\share\\x86_64-windows-ghc-8.8.4\\choplifter-0.1.0.0"
libexecdir = "C:\\Users\\W530\\funktio-projektit\\choplifter\\.stack-work\\install\\8e21f95a\\libexec\\x86_64-windows-ghc-8.8.4\\choplifter-0.1.0.0"
sysconfdir = "C:\\Users\\W530\\funktio-projektit\\choplifter\\.stack-work\\install\\8e21f95a\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "choplifter_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "choplifter_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "choplifter_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "choplifter_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "choplifter_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "choplifter_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
