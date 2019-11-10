{-# LANGUAGE OverloadedStrings #-}

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

module Database.Sqlports
  ( Connection,
    Dependency (..),
    Pkg (..),
    PkgMap,
    close,
    hspkgs,
    allpkgs,
    bydistname,
    open,
    distVersion,
    hackageVersion,
  )
where

import Data.Char (isDigit)
import Data.Function (on)
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Version (Version, parseVersion)
import Database.HDBC
import Database.HDBC.Sqlite3
import System.Process (readProcess)
import Text.ParserCombinators.ReadP (readP_to_S)

data Pkg
  = Pkg
      { fullpkgpath :: Text,
        pkgpath :: Text,
        distname :: Maybe Text,
        pkgname :: Text,
        multi :: Bool,
        deps :: [Dependency]
      }
  deriving (Show, Eq)

type PkgMap = Map Text Pkg -- by fullpkgpath

data DependsType = BuildDepends | LibDepends | RunDepends | RegressDepends
  deriving (Show, Eq)

-- Todo:
-- 1: let Dependency refer to a real Pkg object (recursively)
-- 2: handle dependency alternatives like
--    xfce4-icon-theme-*|tango-icon-theme-*|gnome-icon-theme-*
-- 3: parse pkgspecs and transform them into some usable data type

data Dependency
  = Dependency
      { dependspath :: Text,
        pkgspec :: Text,
        dtype :: DependsType
      }
  deriving (Show, Eq)

dependsType :: Text -> DependsType
dependsType "B" = BuildDepends
dependsType "L" = LibDepends
dependsType "R" = RunDepends
dependsType "Regress" = RegressDepends

open :: IO Connection
open = do
  paths <-
    filter ("/sqlports" `isSuffixOf`) . lines
      <$> readProcess "pkg_info" ["-L", "sqlports"] ""
  case paths of
    [p] -> connectSqlite3 p
    ps -> error "Unexpected pkg_info output"

close :: Connection -> IO ()
close = disconnect

-- Fetch all Pkgs from sqlports, mapped by fullpkgpath.
allpkgs :: Connection -> IO PkgMap
allpkgs = getpkgs ""

-- Fetch all Pkgs depending on lang/ghc from sqlports, mapped by
-- fullpkgpath, with dependencies limited to Pkgs also depending
-- on lang/ghc (i.e.  you won't get dependencies like iconv or gmp).
hspkgs :: Connection -> IO PkgMap
hspkgs c = do
  -- For sqlports (not -compact), this was:
  -- JOIN depends d2 USING (fullpkgpath)
  -- WHERE d2.dependspath = 'lang/ghc'
  pmap <-
    getpkgs
      "JOIN depends d2 USING (fullpkgpath) \
      \JOIN paths pa4 ON d2.dependspath = pa4.FullPkgPath \
      \WHERE pa4.fullpkgpath = 'lang/ghc'"
      c
  return $ pkgClosure pmap

getpkgs :: String -> Connection -> IO PkgMap
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
  stmt <-
    prepare c $
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
        ++ " "
        ++ constr
        ++ " "
        ++ "ORDER BY \
           \  pa1.fullpkgpath, \
           \  pa3.fullpkgpath, \
           \  type"
  execute stmt []
  rows <- fetchAllRows' stmt
  let rowss = groupBy ((==) `on` take 3) rows
  let pkgs = map toPkg rowss
  return $! Map.fromList [(fullpkgpath p, p) | p <- pkgs]
  where
    toPkg rs@((f : p : d : n : m : _) : _) =
      Pkg
        { fullpkgpath = fromSql f,
          pkgpath = fromSql p,
          distname = fromSql d,
          pkgname = fromSql n,
          multi = fromSql m,
          deps = collectdeps (drop 5 <$> rs)
        }
    collectdeps rs =
      let rs' = filter (notElem SqlNull) rs
          toDep [d, s, t] =
            Dependency (fromSql d) (fromSql s) (dependsType (fromSql t))
       in toDep <$> rs'

-- Given a map of fullpkgnames to Pkgs, remove all dependencies
-- contained in Pkgs for which no entry exists in the map.
--
-- Currently used to limit dependencies to Haskell packages only
-- (we aren't interested in dependencies like iconv or gmp).
--
-- For generic code that wants to deal with non-Haskell ports, this
-- function won't be of any use.
--
pkgClosure :: PkgMap -> PkgMap
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
bydistname :: [Pkg] -> PkgMap
bydistname pkgs =
  Map.fromList
    [ (zapVers (fromMaybe undefined dn), p)
      | p <- pkgs,
        let dn = distname p,
        isJust dn,
        not (multi p)
          || (",-lib$" `Text.isSuffixOf` fullpkgpath p)
    ]
  where
    zapVers = fst . splitByVersion

splitByVersion :: Text -> (Text, Text)
splitByVersion t =
  ( Text.init $ Text.dropWhileEnd versionChar t,
    Text.takeWhileEnd versionChar t
  )
  where
    versionChar '.' = True
    versionChar c = isDigit c

-- Extract the version number from a Pkgs distname.
distVersion :: Pkg -> Text
distVersion = snd . splitByVersion . fullpkgpath

-- | Returns the hackage version for the given package if possible.
hackageVersion :: Pkg -> Maybe Version
hackageVersion p =
  let pickFullParse = filter ((== "") . snd)
   in case pickFullParse $ readP_to_S parseVersion $ Text.unpack $ distVersion p of
        [(v, "")] -> Just v
        _ -> Nothing
