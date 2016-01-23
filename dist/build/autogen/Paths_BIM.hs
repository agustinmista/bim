module Paths_BIM (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/agustin/.cabal/bin"
libdir     = "/home/agustin/.cabal/lib/x86_64-linux-ghc-7.8.4/BIM-0.1"
datadir    = "/home/agustin/.cabal/share/x86_64-linux-ghc-7.8.4/BIM-0.1"
libexecdir = "/home/agustin/.cabal/libexec"
sysconfdir = "/home/agustin/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "BIM_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "BIM_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "BIM_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "BIM_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "BIM_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
