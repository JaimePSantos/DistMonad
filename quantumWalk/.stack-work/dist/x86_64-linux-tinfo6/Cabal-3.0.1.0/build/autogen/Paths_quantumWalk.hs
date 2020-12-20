{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_quantumWalk (
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

bindir     = "/home/jaime/Programming/MonadsINESC/quantumWalk/.stack-work/install/x86_64-linux-tinfo6/3997baad4bca4e2ce6ea86085dc6ea654aaae28b47d90cb299fd12375eec8389/8.8.4/bin"
libdir     = "/home/jaime/Programming/MonadsINESC/quantumWalk/.stack-work/install/x86_64-linux-tinfo6/3997baad4bca4e2ce6ea86085dc6ea654aaae28b47d90cb299fd12375eec8389/8.8.4/lib/x86_64-linux-ghc-8.8.4/quantumWalk-0.1.0.0-KCCNRwe0NYwCbIQDJZdyHC"
dynlibdir  = "/home/jaime/Programming/MonadsINESC/quantumWalk/.stack-work/install/x86_64-linux-tinfo6/3997baad4bca4e2ce6ea86085dc6ea654aaae28b47d90cb299fd12375eec8389/8.8.4/lib/x86_64-linux-ghc-8.8.4"
datadir    = "/home/jaime/Programming/MonadsINESC/quantumWalk/.stack-work/install/x86_64-linux-tinfo6/3997baad4bca4e2ce6ea86085dc6ea654aaae28b47d90cb299fd12375eec8389/8.8.4/share/x86_64-linux-ghc-8.8.4/quantumWalk-0.1.0.0"
libexecdir = "/home/jaime/Programming/MonadsINESC/quantumWalk/.stack-work/install/x86_64-linux-tinfo6/3997baad4bca4e2ce6ea86085dc6ea654aaae28b47d90cb299fd12375eec8389/8.8.4/libexec/x86_64-linux-ghc-8.8.4/quantumWalk-0.1.0.0"
sysconfdir = "/home/jaime/Programming/MonadsINESC/quantumWalk/.stack-work/install/x86_64-linux-tinfo6/3997baad4bca4e2ce6ea86085dc6ea654aaae28b47d90cb299fd12375eec8389/8.8.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "quantumWalk_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "quantumWalk_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "quantumWalk_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "quantumWalk_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "quantumWalk_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "quantumWalk_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
