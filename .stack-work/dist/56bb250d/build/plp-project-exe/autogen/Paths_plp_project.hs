{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_plp_project (
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
bindir     = "E:\\ProjetosHaskell\\PLP-project\\.stack-work\\install\\0d39dc4e\\bin"
libdir     = "E:\\ProjetosHaskell\\PLP-project\\.stack-work\\install\\0d39dc4e\\lib\\x86_64-windows-ghc-9.10.2-cea6\\plp-project-0.1.0.0-36sUVdhtjL2ApYKDTKuDh3-plp-project-exe"
dynlibdir  = "E:\\ProjetosHaskell\\PLP-project\\.stack-work\\install\\0d39dc4e\\lib\\x86_64-windows-ghc-9.10.2-cea6"
datadir    = "E:\\ProjetosHaskell\\PLP-project\\.stack-work\\install\\0d39dc4e\\share\\x86_64-windows-ghc-9.10.2-cea6\\plp-project-0.1.0.0"
libexecdir = "E:\\ProjetosHaskell\\PLP-project\\.stack-work\\install\\0d39dc4e\\libexec\\x86_64-windows-ghc-9.10.2-cea6\\plp-project-0.1.0.0"
sysconfdir = "E:\\ProjetosHaskell\\PLP-project\\.stack-work\\install\\0d39dc4e\\etc"

getBinDir     = catchIO (getEnv "plp_project_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "plp_project_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "plp_project_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "plp_project_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "plp_project_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "plp_project_sysconfdir") (\_ -> return sysconfdir)



joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '\\'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/' || c == '\\'
