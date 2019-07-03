{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_real (
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

bindir     = "F:\\Projects\\Haskell\\ohaskell\\real\\.stack-work\\install\\891ce259\\bin"
libdir     = "F:\\Projects\\Haskell\\ohaskell\\real\\.stack-work\\install\\891ce259\\lib\\x86_64-windows-ghc-8.6.5\\real-0.1.0.0-9Mp7rR6mGZyL6WMhcMPRq-real"
dynlibdir  = "F:\\Projects\\Haskell\\ohaskell\\real\\.stack-work\\install\\891ce259\\lib\\x86_64-windows-ghc-8.6.5"
datadir    = "F:\\Projects\\Haskell\\ohaskell\\real\\.stack-work\\install\\891ce259\\share\\x86_64-windows-ghc-8.6.5\\real-0.1.0.0"
libexecdir = "F:\\Projects\\Haskell\\ohaskell\\real\\.stack-work\\install\\891ce259\\libexec\\x86_64-windows-ghc-8.6.5\\real-0.1.0.0"
sysconfdir = "F:\\Projects\\Haskell\\ohaskell\\real\\.stack-work\\install\\891ce259\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "real_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "real_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "real_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "real_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "real_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "real_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
