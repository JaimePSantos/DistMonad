{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_distMonad (
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

bindir     = "/home/jaime/Programming/MonadsINESC/distMonad/.stack-work/install/x86_64-linux-tinfo6/b48c1b48ed44d7a434ad78f4631e97c55dd8305170d75c46feefc4bb855eb771/8.8.3/bin"
libdir     = "/home/jaime/Programming/MonadsINESC/distMonad/.stack-work/install/x86_64-linux-tinfo6/b48c1b48ed44d7a434ad78f4631e97c55dd8305170d75c46feefc4bb855eb771/8.8.3/lib/x86_64-linux-ghc-8.8.3/distMonad-0.1.0.0-5hQ0a4fCKze7osgIUMVKqS"
dynlibdir  = "/home/jaime/Programming/MonadsINESC/distMonad/.stack-work/install/x86_64-linux-tinfo6/b48c1b48ed44d7a434ad78f4631e97c55dd8305170d75c46feefc4bb855eb771/8.8.3/lib/x86_64-linux-ghc-8.8.3"
datadir    = "/home/jaime/Programming/MonadsINESC/distMonad/.stack-work/install/x86_64-linux-tinfo6/b48c1b48ed44d7a434ad78f4631e97c55dd8305170d75c46feefc4bb855eb771/8.8.3/share/x86_64-linux-ghc-8.8.3/distMonad-0.1.0.0"
libexecdir = "/home/jaime/Programming/MonadsINESC/distMonad/.stack-work/install/x86_64-linux-tinfo6/b48c1b48ed44d7a434ad78f4631e97c55dd8305170d75c46feefc4bb855eb771/8.8.3/libexec/x86_64-linux-ghc-8.8.3/distMonad-0.1.0.0"
sysconfdir = "/home/jaime/Programming/MonadsINESC/distMonad/.stack-work/install/x86_64-linux-tinfo6/b48c1b48ed44d7a434ad78f4631e97c55dd8305170d75c46feefc4bb855eb771/8.8.3/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "distMonad_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "distMonad_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "distMonad_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "distMonad_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "distMonad_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "distMonad_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
