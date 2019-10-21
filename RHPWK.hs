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
import Control.Arrow (first)
import Control.Exception (bracket)
import Control.Monad (forM_)
import Data.List (isSuffixOf, nub, sort)
import Data.Map (Map)
import Data.Maybe
import Database.GhcPkg
import Database.Sqlports
import Distribution.InstalledPackageInfo
import Distribution.Package
import Distribution.Pretty (prettyShow)
import Distribution.Types.Version (mkVersion')
import Prelude hiding (lookup)
import System.Console.GetOpt
import System.Environment
import System.FilePath
import qualified Data.Map as Map
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
		(fs@(_:_), False, False, False) ->	packageList fs
		_	      ->	ioError (userError usage)


packageList fs = do
  hpkgs <- bracket open close hspkgs
  ips <- installedpkgs
  hdb <- readHackage
  mapM_ (processFile ips hpkgs hdb) fs

readHackage :: IO DB.HackageDB
readHackage = DB.hackageTarball >>= DB.readTarball Nothing

ppkgpath :: Pkg -> String
ppkgpath = Database.Sqlports.pkgpath

ipkgpath :: InstalledPackageInfo -> String
ipkgpath = const "TODO: ipkgpath"

dumpWith :: (Connection -> IO (Map String Pkg)) -> IO ()
dumpWith f = do
	ps <- bracket open close f
	putStr $ unlines $ map show $ Map.elems ps

dumpPkgs :: Bool -> IO ()
dumpPkgs all = do
	ipkgs <- installedpkgs
	let ghcpkgs = [ p | p <- Map.elems ipkgs, all || ipkgpath p == "lang/ghc" ]
	putStr $ unlines $ map show $ ghcpkgs

processFile ::    InstalledPackages
	       -> Map String Pkg
	       -> DB.HackageDB
	       -> String
	       -> IO ()
processFile ipkgs hpkgs hdb f = do
  if ".cabal" `isSuffixOf` f
    then do
      systemPkgs <- Map.keysSet <$> bundledPackages
      dumpCabalDeps systemPkgs f
    else findPkg ipkgs hpkgs hdb f

findPkg ::    InstalledPackages
	   -> Map String Pkg
	   -> DB.HackageDB
	   -> String
	   -> IO ()
findPkg ipkgs hpkgs hdb p = do
  print p
  let pkgs' = bydistname $ Map.elems hpkgs
      printJust x f = maybe (pure ()) (putStrLn . f) x
  printJust (Map.lookup p pkgs') $ \pkg ->
    "sqlports:\t" <> (fullpkgpath pkg) <> " (" <> distVersion pkg <> ")"
  printJust (Map.lookup (mkPackageName p) ipkgs) $ \pkg ->
    "ghc-pkg:\t" <> (ipkgpath pkg) <>
    " (" <> (prettyShow $ pkgVersion $ sourcePackageId pkg) <> ")"
  printJust (Map.lookup (mkPackageName p) hdb) $ \pkg ->
    "hackage:\t" <> latestFromPackageHackage pkg

latestFromPackageHackage :: DB.PackageData -> String
latestFromPackageHackage m = fromMaybe "not found" latest
  where latest =
          prettyShow . pkgVersion . PD.package . PD.packageDescription
          . DB.cabalFile . fst <$> Map.maxView m

-- | Appends updated dependency information based on hackage
-- declarations into the Makefiles. The ports become unbuildable but
-- somewhat convenient to update manually.
--
-- TODO: compare the pre-existing declarations with the computed
-- ones to save resolution work on human's part.
printHackageDeps :: IO ()
printHackageDeps = do
  hpkgs <- bracket open close hspkgs
  hdb <- readHackage
  systemPkgs <- Map.keysSet <$> bundledPackages
  let pkgs = bydistname $ Map.elems hpkgs
  forM_ (Map.assocs pkgs) $ \(pname, hpkg) -> do
      let hv = mkVersion'
               $ fromMaybe (error $ "Can't determine hackage version of " <> pname)
               $ hackageVersion hpkg
      putStrLn $ fullpkgpath hpkg <> " " <> pname <> "-" <> prettyShow hv
      case Map.lookup (mkPackageName pname) hdb of
        Nothing -> putStrLn $ "Not in hackage " <> pname
        Just pkgData ->
          case Map.lookup hv pkgData of
            Nothing -> putStrLn $ "No version data " <> prettyShow hv
            Just verData -> do
              let pkgPd = DB.cabalFile verData
                  pkg = either (error . show) id $ refineDescription pkgPd
              let frags =
                    filter (not . null . snd) $  -- Bug in dumpCabalDeps
                    dumpDepsFromPD systemPkgs pkg
                  -- The ??? below happen when pointing to non-existent ports.
                  -- They aren't necessarily errors because the port may not
                  -- be built in with flags that require such a dependency.
                  packetizeName p = maybe ("???" <> p) fullpkgpath $ Map.lookup p pkgs
                  depListing = unlines $ mconcat [
                    what <> "\t\t= \\" :
                    [ "\t\t\t" <> hp <> concat rs <> " \\"
                    | (hp, rs) <- sort (first packetizeName <$> nub ps)
                    ]
                    | (what, ps) <- frags
                    ]
              if (not $ null depListing)
                then do
                   let name = "/usr/ports" </> pkgpath hpkg </> "Makefile"
                   appendFile name depListing
                   putStrLn $ "Appended to " <> name
                else putStrLn "Nothing to do"
