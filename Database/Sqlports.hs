-- Copyright (c) 2010 Matthias Kilian <kili@outback.escape.de>
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

{-# LANGUAGE CPP #-}

module Database.Sqlports (
	Dependency (..),
	Pkg (..),
	close,
	hspkgs,
	allpkgs,
	open
) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Database.HDBC
import Database.HDBC.Sqlite3
import System.IO

data Pkg = Pkg {
	fullpkgpath :: String,
	pkgpath :: String,
	distname :: Maybe String,
	deps :: [Dependency]
} deriving (Show, Eq)

data DependsType = BuildDepends | LibDepends | RunDepends | RegressDepends
	deriving (Show, Eq)

-- Todo:
-- 1: let Dependency refer to a real Pkg object (recursively)
-- 2: handle dependency alternatives like
--    xfce4-icon-theme-*|tango-icon-theme-*|gnome-icon-theme-*
-- 3: parse pkgspecs and transform them into some usable data type

data Dependency = Dependency {
	dependspath :: String,
	pkgspec :: String,
	dtype :: DependsType
} deriving (Show, Eq)

dependsType "B" = BuildDepends
dependsType "L" = LibDepends
dependsType "R" = RunDepends
dependsType "Regress" = RegressDepends

open :: IO Connection
open = connectSqlite3 SQLPORTSPATH

close :: Connection -> IO ()
close = disconnect

hspkgdeps :: Connection -> Pkg -> IO Pkg
hspkgdeps c p = do
		stmt <- prepare c "SELECT dependspath, pkgspec, type \\
				  \FROM depends \\
				  \WHERE fullpkgpath = ?"
		execute stmt [toSql $ fullpkgpath p]
		rows <- fetchAllRows' stmt
		let ds = map (toDep . \[d, s, t] -> (fromSql d, fromSql s, fromSql t)) rows
		return p {deps = ds}
	where
		toDep (d, s, t) = Dependency d s (dependsType t)

hspkgs :: Connection -> IO (Map String Pkg)
hspkgs c = do
		stmt <- prepare c "SELECT DISTINCT pa.fullpkgpath, pkgpath, distname \\
				  \FROM depends d \\
				  \JOIN paths pa ON d.fullpkgpath = pa.fullpkgpath \\
				  \JOIN ports po ON pa.fullpkgpath = po.fullpkgpath \\
				  \WHERE dependspath ='lang/ghc' \\
				  \AND TYPE in ('B', 'L', 'R')"
		execute stmt []
		rows <- fetchAllRows' stmt
		let pkg0s = map (toPkg . \[f, p, d] -> (fromSql f, fromSql p, fromSql d)) rows
		pkg1s <- mapM (hspkgdeps c) pkg0s
		let pmap = Map.fromList $ [(fullpkgpath p, p) | p <- pkg1s]
		return $ pkgClosure pmap

-- Just for testing wether it also works in the generic (Non-haskell) case:
allpkgs :: Connection -> IO (Map String Pkg)
allpkgs c = do
		stmt <- prepare c "SELECT DISTINCT fullpkgpath, pkgpath, distname \\
				  \FROM paths JOIN ports USING (fullpkgpath)"
		execute stmt []
		rows <- fetchAllRows' stmt
		hPutStrLn stderr "Reading paths..."
		let pkg0s = map (toPkg . \[f, p, d] -> (fromSql f, fromSql p, fromSql d)) rows
		hPutStrLn stderr "Reading deps..."
                -- XXX very inefficient, because it executes several
                -- thousands queries on the database. Better combine
                -- dependency fetching with the package fetching
                -- above ordered by fullpkgpath and assemble the
                -- packages's dependencies on the fly.
		pkg1s <- mapM (hspkgdeps c) pkg0s
		hPutStrLn stderr "Creating map..."
		let pmap = Map.fromList $ [(fullpkgpath p, p) | p <- pkg1s]
		hPutStrLn stderr "Done."
		return pmap

toPkg (f, p, d) = Pkg f p d undefined

-- Given a map of fullpkgnames to Pkgs, remove all dependencies
-- contained in Pkgs for which no entry exists in the map.
--
-- Currently used to limit dependencies to Haskell packages only
-- (we aren't interested in dependencies like iconv or gmp).
--
-- For generic code that wants to deal with non-Haskell ports, this
-- function won't be of any use.
--
pkgClosure :: Map String Pkg -> Map String Pkg
pkgClosure ps = Map.map zapNonHsDeps ps
	where
		zapNonHsDeps :: Pkg -> Pkg
		zapNonHsDeps p = p {deps = filter isHsDep $ deps p}
		isHsDep :: Dependency -> Bool
		isHsDep d = dependspath d `Map.member` ps
