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

bindir     = "D:\\Programming\\Monads\\MonadsINESC\\distMonad\\.stack-work\\install\\dc761ad7\\bin"
libdir     = "D:\\Programming\\Monads\\MonadsINESC\\distMonad\\.stack-work\\install\\dc761ad7\\lib\\x86_64-windows-ghc-8.8.3\\distMonad-0.1.0.0-4BF8SilX4oREVg5N4xXnez-distMonad"
dynlibdir  = "D:\\Programming\\Monads\\MonadsINESC\\distMonad\\.stack-work\\install\\dc761ad7\\lib\\x86_64-windows-ghc-8.8.3"
datadir    = "D:\\Programming\\Monads\\MonadsINESC\\distMonad\\.stack-work\\install\\dc761ad7\\share\\x86_64-windows-ghc-8.8.3\\distMonad-0.1.0.0"
libexecdir = "D:\\Programming\\Monads\\MonadsINESC\\distMonad\\.stack-work\\install\\dc761ad7\\libexec\\x86_64-windows-ghc-8.8.3\\distMonad-0.1.0.0"
sysconfdir = "D:\\Programming\\Monads\\MonadsINESC\\distMonad\\.stack-work\\install\\dc761ad7\\etc"

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
  return (dir ++ "\\" ++ name)
