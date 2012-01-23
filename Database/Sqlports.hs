-- Copyright (c) 2010, 2012 Matthias Kilian <kili@outback.escape.de>
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

module Database.Sqlports (
	Dependency (..),
	Pkg (..),
	close,
	hspkgs,
	allpkgs,
	bydistname,
	open
) where

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Database.HDBC
import Database.HDBC.Sqlite3
import System.IO
import Text.Regex

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

allpkgs :: Connection -> IO (Map String Pkg)
allpkgs = getpkgs ""

hspkgs :: Connection -> IO (Map String Pkg)
hspkgs c = do
		pmap <- getpkgs "JOIN depends d2 USING (fullpkgpath) \\
				\WHERE d2.dependspath = 'lang/ghc'"
				c
		return $ pkgClosure pmap

getpkgs :: String -> Connection -> IO (Map String Pkg)
getpkgs constr c = do
		stmt <- prepare c $
			  "SELECT DISTINCT \\
			  \fullpkgpath, pkgpath, distname, \\
			  \d.dependspath, d.pkgspec, d.type \\
			  \FROM paths \\
			  \JOIN ports USING (fullpkgpath) \\
			  \LEFT JOIN depends d USING (fullpkgpath) "
			  ++ constr ++
			  " ORDER BY fullpkgpath, d.dependspath, d.type"
		execute stmt []
		rows <- fetchAllRows' stmt
		let rowss = groupBy (\rs rs' -> take 3 rs == take 3 rs') rows
		let pkgs = map toPkg rowss
		let pmap = Map.fromList [(fullpkgpath p, p) | p <- pkgs]
		return pmap
	where

		toPkg rs@([f, p, d, _, _, _] : _) =
			let f' = fromSql f
			    p' = fromSql p
			    d' = fromSql d
			in
			    collectdeps (Pkg f' p' d' undefined) (map (drop 3) rs)

		collectdeps p rs =
			let rs' = filter (all (/= SqlNull)) rs
			    toDep (d, s, t) = Dependency d s (dependsType t)
			    ds = map (toDep . \[d, s, t] -> (fromSql d, fromSql s, fromSql t)) rs'
			in p {deps = ds}

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


-- Build a map from a list of packages where the keys are distnames (but
-- without the version number).
bydistname :: [Pkg] -> Map String Pkg
bydistname pkgs = Map.fromList [ (zapVers (fromMaybe "" dn), p) | p <- pkgs, let dn = distname p, isJust dn]
	where
		zapVers s = subRegex (mkRegex "-[0-9.]*$") s ""
