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

module Database.GhcPkg (
	installedpkgs
) where

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Distribution.InstalledPackageInfo
import Distribution.Package
import Distribution.Simple.Utils
import System.Directory
import System.FilePath

-- Fetch all InstalledPackagesInfos known to ghc (or ghc-pkg),
-- mapped by the PackageName.
installedpkgs :: IO (Map String InstalledPackageInfo)
installedpkgs = do
	let path = "/usr/local/lib/ghc/package.conf.d"
	fs <- getDirectoryContents path
	let confs = filter (".conf" `isSuffixOf`) fs
	pkgs <- mapM parsePkgInfo $ map (path </>) confs
	return $ Map.fromList [ (unPkgName $ pkgName $ sourcePackageId p, p) | p <- pkgs ]
	where
		unPkgName = unPackageName

parsePkgInfo :: FilePath -> IO InstalledPackageInfo
parsePkgInfo f = do
	c <- readUTF8File f
	case parseInstalledPackageInfo c of
		ParseOk _ ps  ->	return ps
