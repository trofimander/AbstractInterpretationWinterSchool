module Paths_abstract_interpretation (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/Users/traff/Library/Haskell/ghc-7.6.3/lib/abstract-interpretation-0.0.0/bin"
libdir     = "/Users/traff/Library/Haskell/ghc-7.6.3/lib/abstract-interpretation-0.0.0/lib"
datadir    = "/Users/traff/Library/Haskell/ghc-7.6.3/lib/abstract-interpretation-0.0.0/share"
libexecdir = "/Users/traff/Library/Haskell/ghc-7.6.3/lib/abstract-interpretation-0.0.0/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "abstract_interpretation_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "abstract_interpretation_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "abstract_interpretation_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "abstract_interpretation_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
