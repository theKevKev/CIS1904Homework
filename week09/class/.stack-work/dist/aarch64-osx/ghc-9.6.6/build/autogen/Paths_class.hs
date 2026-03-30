{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_class (
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
bindir     = "/Users/home/CIS1904Homework/week09/class/.stack-work/install/aarch64-osx/6d992e0e806b07d9b76f43549f5ed92a46d89eaa890d1fe79b7a78fb2b10f501/9.6.6/bin"
libdir     = "/Users/home/CIS1904Homework/week09/class/.stack-work/install/aarch64-osx/6d992e0e806b07d9b76f43549f5ed92a46d89eaa890d1fe79b7a78fb2b10f501/9.6.6/lib/aarch64-osx-ghc-9.6.6/class-0.1.0.0-589QKrFFNuk3GD0gcch2ye"
dynlibdir  = "/Users/home/CIS1904Homework/week09/class/.stack-work/install/aarch64-osx/6d992e0e806b07d9b76f43549f5ed92a46d89eaa890d1fe79b7a78fb2b10f501/9.6.6/lib/aarch64-osx-ghc-9.6.6"
datadir    = "/Users/home/CIS1904Homework/week09/class/.stack-work/install/aarch64-osx/6d992e0e806b07d9b76f43549f5ed92a46d89eaa890d1fe79b7a78fb2b10f501/9.6.6/share/aarch64-osx-ghc-9.6.6/class-0.1.0.0"
libexecdir = "/Users/home/CIS1904Homework/week09/class/.stack-work/install/aarch64-osx/6d992e0e806b07d9b76f43549f5ed92a46d89eaa890d1fe79b7a78fb2b10f501/9.6.6/libexec/aarch64-osx-ghc-9.6.6/class-0.1.0.0"
sysconfdir = "/Users/home/CIS1904Homework/week09/class/.stack-work/install/aarch64-osx/6d992e0e806b07d9b76f43549f5ed92a46d89eaa890d1fe79b7a78fb2b10f501/9.6.6/etc"

getBinDir     = catchIO (getEnv "class_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "class_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "class_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "class_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "class_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "class_sysconfdir") (\_ -> return sysconfdir)



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
