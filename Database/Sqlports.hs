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
	Connection,
	Dependency (..),
	Pkg (..),
	close,
	hspkgs,
	allpkgs,
	bydistname,
	open,
	distVersion
) where

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Database.HDBC
import Database.HDBC.Sqlite3
import Text.Regex

data Pkg = Pkg {
	fullpkgpath :: String,
	pkgpath :: String,
	distname :: Maybe String,
	pkgname :: String,
	multi :: Bool,
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
open = connectSqlite3 "/usr/local/share/sqlports"

close :: Connection -> IO ()
close = disconnect

-- Fetch all Pkgs from sqlports, mapped by fullpkgpath.
allpkgs :: Connection -> IO (Map String Pkg)
allpkgs = getpkgs ""

-- Fetch all Pkgs depending on lang/ghc from sqlports, mapped by
-- fullpkgpath, with dependencies limited to Pkgs also depending
-- on lang/ghc (i.e.  you won't get dependencies like iconv or gmp).
hspkgs :: Connection -> IO (Map String Pkg)
hspkgs c = do
		-- For sqlports (not -compact), this was:
		-- JOIN depends d2 USING (fullpkgpath)
		-- WHERE d2.dependspath = 'lang/ghc'
		pmap <- getpkgs "JOIN depends d2 USING (fullpkgpath) \
				\JOIN paths pa4 ON d2.dependspath = pa4.FullPkgPath \
				\WHERE pa4.fullpkgpath = 'lang/ghc'"
				c
		return $ pkgClosure pmap

getpkgs :: String -> Connection -> IO (Map String Pkg)
getpkgs constr c = do
		-- For sqlports (not -compact), this was:
		-- SELECT DISTINCT
		--   ports.fullpkgpath,
		--   paths.pkgpath,
		--   ports.distname,
		--   ports.multi_packages IS NOT NULL as multi,
		--   d.dependspath,
		--   d.pkgspec,
		--   d.type
		-- FROM ports
		-- JOIN paths USING (fullpkgpath)
		-- LEFT JOIN depends d USING (fullpkgpath)
		-- <++ constr ++>
		-- ORDER BY fullpkgpath, d.dependspath, d.type;
		stmt <- prepare c $
			"SELECT DISTINCT \
			\  pa1.fullpkgpath, \
			\  pa2.fullpkgpath as pkgpath, \
			\  ports.distname, \
			\  ports.pkgname, \
			\  multi.fullpkgpath IS NOT NULL as multi, \
			\  pa3.fullpkgpath as dependspath, \
			\  d.pkgspec, \
			\  CASE d.type \
			\    WHEN 0 THEN 'L' \
			\    WHEN 1 THEN 'R' \
			\    WHEN 2 THEN 'B' \
			\    WHEN 3 THEN 'Regress' \
			\  END as type \
			\FROM ports \
			\JOIN paths pa1 ON ports.fullpkgpath = pa1.FullPkgPath \
			\JOIN paths pa2 ON pa1.pkgpath = pa2.FullPkgPath \
			\LEFT JOIN multi ON ports.fullpkgpath = multi.fullpkgpath \
			\LEFT JOIN depends d USING (fullpkgpath) \
			\LEFT JOIN paths pa3 ON d.dependspath = pa3.FullPkgPath"
			++ " " ++ constr ++ " " ++
			"ORDER BY \
			\  pa1.fullpkgpath, \
			\  pa3.fullpkgpath, \
			\  type"
		execute stmt []
		rows <- fetchAllRows' stmt
		let rowss = groupBy (\rs rs' -> take 3 rs == take 3 rs') rows
		let pkgs = map toPkg rowss
		let pmap = Map.fromList [(fullpkgpath p, p) | p <- pkgs]
		return pmap
	where

		toPkg rs@([f, p, d, n, m, _, _, _] : _) =
			let f' = fromSql f
			    p' = fromSql p
			    d' = fromSql d
			    n' = fromSql n
			    m' = fromSql m
			in
			    collectdeps (Pkg f' p' d' n' m' undefined) (map (drop 5) rs)

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


-- Build a map from a list of Pkgs where the keys are distnames (but
-- without the version number). For multipackages, only take the
-- ",-lib" subpackage (probably wrong, but currently, all hs-ports
-- with multipackage actually have a -main and a -lib subpackage).
bydistname :: [Pkg] -> Map String Pkg
bydistname pkgs = Map.fromList [ (zapVers (fromMaybe undefined dn), p)
			       | p <- pkgs,
				 let dn = distname p,
				 isJust dn,
				 not (multi p) ||
				 isJust (matchRegex (mkRegex ",-lib$") (fullpkgpath p))
			       ]
	where
		zapVers s = subRegex (mkRegex "-[0-9.]*$") s ""

-- Extract the version number from a Pkgs distnme.
distVersion :: Pkg -> String
distVersion = xv . fromJust . distname
	where
		xv = head . fromJust . matchRegex (mkRegex "-([0-9.]*)$")
