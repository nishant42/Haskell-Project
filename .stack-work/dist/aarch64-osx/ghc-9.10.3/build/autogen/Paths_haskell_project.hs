{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_haskell_project (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/Users/nishantkumar/Desktop/functional/Haskell-Project/.stack-work/install/aarch64-osx/61065a1375a329d195ba54c8cde6271b1004f10744c1695f3fb5c10ddb71dcf8/9.10.3/bin"
libdir     = "/Users/nishantkumar/Desktop/functional/Haskell-Project/.stack-work/install/aarch64-osx/61065a1375a329d195ba54c8cde6271b1004f10744c1695f3fb5c10ddb71dcf8/9.10.3/lib/aarch64-osx-ghc-9.10.3-fe9c/haskell-project-0.1.0.0-CC5ng7wihT29SSkE1UM92A"
dynlibdir  = "/Users/nishantkumar/Desktop/functional/Haskell-Project/.stack-work/install/aarch64-osx/61065a1375a329d195ba54c8cde6271b1004f10744c1695f3fb5c10ddb71dcf8/9.10.3/lib/aarch64-osx-ghc-9.10.3-fe9c"
datadir    = "/Users/nishantkumar/Desktop/functional/Haskell-Project/.stack-work/install/aarch64-osx/61065a1375a329d195ba54c8cde6271b1004f10744c1695f3fb5c10ddb71dcf8/9.10.3/share/aarch64-osx-ghc-9.10.3-fe9c/haskell-project-0.1.0.0"
libexecdir = "/Users/nishantkumar/Desktop/functional/Haskell-Project/.stack-work/install/aarch64-osx/61065a1375a329d195ba54c8cde6271b1004f10744c1695f3fb5c10ddb71dcf8/9.10.3/libexec/aarch64-osx-ghc-9.10.3-fe9c/haskell-project-0.1.0.0"
sysconfdir = "/Users/nishantkumar/Desktop/functional/Haskell-Project/.stack-work/install/aarch64-osx/61065a1375a329d195ba54c8cde6271b1004f10744c1695f3fb5c10ddb71dcf8/9.10.3/etc"

getBinDir     = catchIO (getEnv "haskell_project_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "haskell_project_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "haskell_project_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "haskell_project_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "haskell_project_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "haskell_project_sysconfdir") (\_ -> return sysconfdir)



joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
