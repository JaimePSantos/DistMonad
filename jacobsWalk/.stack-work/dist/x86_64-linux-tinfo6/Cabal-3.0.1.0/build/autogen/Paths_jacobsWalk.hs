{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_jacobsWalk (
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

bindir     = "/home/jaime/Programming/MonadsINESC/jacobsWalk/.stack-work/install/x86_64-linux-tinfo6/cd1d7e8210661fc8e51b68eeb8fe187ad8bf6706652df44f504b56c371d671aa/8.8.4/bin"
libdir     = "/home/jaime/Programming/MonadsINESC/jacobsWalk/.stack-work/install/x86_64-linux-tinfo6/cd1d7e8210661fc8e51b68eeb8fe187ad8bf6706652df44f504b56c371d671aa/8.8.4/lib/x86_64-linux-ghc-8.8.4/jacobsWalk-0.1.0.0-JBNhUAIFEkdFqWVCvhZNaK"
dynlibdir  = "/home/jaime/Programming/MonadsINESC/jacobsWalk/.stack-work/install/x86_64-linux-tinfo6/cd1d7e8210661fc8e51b68eeb8fe187ad8bf6706652df44f504b56c371d671aa/8.8.4/lib/x86_64-linux-ghc-8.8.4"
datadir    = "/home/jaime/Programming/MonadsINESC/jacobsWalk/.stack-work/install/x86_64-linux-tinfo6/cd1d7e8210661fc8e51b68eeb8fe187ad8bf6706652df44f504b56c371d671aa/8.8.4/share/x86_64-linux-ghc-8.8.4/jacobsWalk-0.1.0.0"
libexecdir = "/home/jaime/Programming/MonadsINESC/jacobsWalk/.stack-work/install/x86_64-linux-tinfo6/cd1d7e8210661fc8e51b68eeb8fe187ad8bf6706652df44f504b56c371d671aa/8.8.4/libexec/x86_64-linux-ghc-8.8.4/jacobsWalk-0.1.0.0"
sysconfdir = "/home/jaime/Programming/MonadsINESC/jacobsWalk/.stack-work/install/x86_64-linux-tinfo6/cd1d7e8210661fc8e51b68eeb8fe187ad8bf6706652df44f504b56c371d671aa/8.8.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "jacobsWalk_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "jacobsWalk_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "jacobsWalk_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "jacobsWalk_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "jacobsWalk_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "jacobsWalk_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
