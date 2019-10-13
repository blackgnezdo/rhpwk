-- Copyright (c) 2010,2012 Matthias Kilian <kili@outback.escape.de>
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

module RHPWK (main) where

import Cabal.Cabal
import Data.List (isSuffixOf)
import Data.Map hiding (map)
import Data.Maybe
import Database.GhcPkg
import Database.Sqlports
import Distribution.InstalledPackageInfo
import Distribution.Package
import Distribution.Pretty (prettyShow)
import Prelude hiding (lookup)
import System.Console.GetOpt
import System.Environment
import qualified Distribution.Hackage.DB as DB
import qualified Distribution.PackageDescription as PD

data Flag = All | Dump | Pkgs deriving (Eq, Show)

options :: [OptDescr Flag]
options =
	[ Option ['a']	[]	(NoArg All)		"for -d and -p, dump all data"
	, Option ['d']	[]	(NoArg Dump)		"dump sqlports data"
	, Option ['p']	[]	(NoArg Pkgs)		"dump installed package data"
	]

usage = usageInfo "Usage: rhpwk [-d [-a]] | [ -p [-a]] | pname ..." options

parseOpts :: [String] -> IO ([Flag], [String])
parseOpts argv =
	case getOpt Permute options argv of
		(o, n, []) ->	return (o, n)
		(_, _, es) ->	ioError (userError usage)


main = do
	argv <- getArgs
	(flags, args) <- parseOpts argv
	case (args, Dump `elem` flags, All `elem` flags, Pkgs `elem` flags) of
		([], True, False, False)	->	dumpWith hspkgs
		([], True, True, False)		->	dumpWith allpkgs
		([], False, all, True)		->	dumpPkgs all
		(fs@(_:_), False, False, False) ->	do
								c <- open
								hps <- hspkgs c
								close c
								ips <- installedpkgs
								hdb <- readHackage
								mapM_ (processFile ips hps hdb) fs
		_	      ->	ioError (userError usage)


readHackage :: IO DB.HackageDB
readHackage = DB.hackageTarball >>= DB.readTarball Nothing

ppkgpath :: Pkg -> String
ppkgpath = Database.Sqlports.pkgpath

ipkgpath :: InstalledPackageInfo -> String
ipkgpath = const "TODO: ipkgpath"

dumpWith :: (Connection -> IO (Map String Pkg)) -> IO ()
dumpWith f = do
	c <- open
	ps <- f c
	close c
	putStr $ unlines $ map show $ elems ps

dumpPkgs :: Bool -> IO ()
dumpPkgs all = do
	ipkgs <- installedpkgs
	let ghcpkgs = [ p | p <- elems ipkgs, all || ipkgpath p == "lang/ghc" ]
	mapM_ (putStrLn . prettyShow . sourcePackageId) ghcpkgs

processFile ::    Map String InstalledPackageInfo
	       -> Map String Pkg
	       -> DB.HackageDB
	       -> String
	       -> IO ()
processFile ipkgs hpkgs hdb f =
	if ".cabal" `isSuffixOf` f
	then dumpCabalDeps f
	else findPkg ipkgs hpkgs hdb f

findPkg ::    Map String InstalledPackageInfo
	   -> Map String Pkg
	   -> DB.HackageDB
	   -> String
	   -> IO ()
findPkg ipkgs hpkgs hdb p = do
  print p
  let pkgs' = bydistname $ elems hpkgs
      printJust x f = maybe (pure ()) (putStrLn . f) x
  printJust (lookup p pkgs') $ \pkg ->
    "sqlports:\t" <> (fullpkgpath pkg) <> " (" <> (distVersion pkg) <> ")"
  printJust (lookup p ipkgs) $ \pkg ->
    "ghc-pkg:\t" <> (ipkgpath pkg) <>
    " (" <> (prettyShow $ pkgVersion $ sourcePackageId pkg) <> ")"
  printJust (lookup (mkPackageName p) hdb) $ \pkg ->
    "hackage:\t" <> latestFromPackageHackage pkg

latestFromPackageHackage :: DB.PackageData -> String
latestFromPackageHackage m = fromMaybe "not found" latest
  where latest =
          prettyShow . pkgVersion . PD.package . PD.packageDescription
          . DB.cabalFile . fst <$> maxView m
