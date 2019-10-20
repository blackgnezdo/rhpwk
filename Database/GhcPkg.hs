-- Copyright (c) 2012 Matthias Kilian <kili@outback.escape.de>
--
-- Permission to use, copy, modify, and distribute this software for any
-- purpose with or without fee is hereby granted, provided that the above
-- copyright notice and this permission notice appear in all copies.
--
-- THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
-- WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
-- MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
-- ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
-- WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
-- ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
-- OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

module Database.GhcPkg
  ( InstalledPackages
  , bundledPackages
  , installedpkgs
  ) where

import Data.List
import Data.Map (Map)
import Data.Maybe (listToMaybe)
import qualified Data.Map as Map
import Distribution.InstalledPackageInfo
import Distribution.Package
import Distribution.Simple.Utils
import System.Directory
import System.FilePath
import System.Process (readProcess)

type InstalledPackages = Map PackageName InstalledPackageInfo

-- Fetch all InstalledPackagesInfos known to ghc (or ghc-pkg),
-- mapped by the PackageName.
installedpkgs :: IO InstalledPackages
installedpkgs = do
  top <- listToMaybe <$> confDirPathsInGhc
  let path = maybe (error "pkg_info -L ghc failed") takeDirectory top
  fs <- getDirectoryContents path
  parseConfigFiles $ map (path </>) $ filter (".conf" `isSuffixOf`) fs

parseConfigFiles :: [FilePath] -> IO InstalledPackages
parseConfigFiles confs = do
  pkgs <- mapM (fmap parsePkgInfo . readUTF8File) confs
  return $ Map.fromList [ (pkgName $ sourcePackageId p, p)
                        | p <- pkgs ]
  where parsePkgInfo f =
          case parseInstalledPackageInfo f of
            ParseOk _ ps  -> ps
            x -> error $ "Unable to parse " ++ show x

confDirPathsInGhc :: IO [FilePath]
confDirPathsInGhc =
  filter ("/package.conf.d/" `isInfixOf`) . lines
  <$> readProcess "pkg_info" ["-L", "ghc"] ""

-- | Fetches all InstalledPackagesInfos bundled into the ghc package
-- currently installed on the system.
bundledPackages :: IO InstalledPackages
bundledPackages = do
  fs <- filter (".conf" `isSuffixOf`) <$> confDirPathsInGhc
  parseConfigFiles fs
