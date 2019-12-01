{-# LANGUAGE OverloadedStrings #-}

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

module RHPWK
  ( main,
  )
where

import Cabal.Cabal
import Control.Exception (bracket)
import Data.List (isSuffixOf)
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Database.GhcPkg
import Database.Sqlports
import qualified Distribution.Hackage.DB as DB
import Distribution.InstalledPackageInfo
import Distribution.Package
import qualified Distribution.PackageDescription as PD
import Distribution.Pretty (prettyShow)
import System.Console.GetOpt
import System.Environment
import Prelude

data Flag = All | Dump | Pkgs deriving (Eq, Show)

options :: [OptDescr Flag]
options =
  [ Option ['a'] [] (NoArg All) "for -d and -p, dump all data",
    Option ['d'] [] (NoArg Dump) "dump sqlports data",
    Option ['p'] [] (NoArg Pkgs) "dump installed package data"
  ]

usage = usageInfo "Usage: rhpwk [-d [-a]] | [ -p [-a]] | pname ..." options

parseOpts :: [String] -> IO ([Flag], [String])
parseOpts argv =
  case getOpt Permute options argv of
    (o, n, []) -> return (o, n)
    (_, _, es) -> ioError (userError usage)

main = do
  argv <- getArgs
  (flags, args) <- parseOpts argv
  case (args, Dump `elem` flags, All `elem` flags, Pkgs `elem` flags) of
    ([], True, False, False) -> dumpWith hspkgs
    ([], True, True, False) -> dumpWith allpkgs
    ([], False, all, True) -> dumpPkgs all
    (fs@(_ : _), False, False, False) -> packageList fs
    _ -> ioError (userError usage)

packageList fs = do
  hpkgs <- bracket open close hspkgs
  ips <- installedpkgs
  hdb <- readHackage
  mapM_ (processFile ips hpkgs hdb) fs

readHackage :: IO DB.HackageDB
readHackage = DB.hackageTarball >>= DB.readTarball Nothing

ipkgpath :: InstalledPackageInfo -> String
ipkgpath = const "TODO: ipkgpath"

dumpWith :: (Connection -> IO PkgMap) -> IO ()
dumpWith f = do
  ps <- bracket open close f
  putStr $ unlines $ map show $ Map.elems ps

dumpPkgs :: Bool -> IO ()
dumpPkgs all = do
  ipkgs <- installedpkgs
  let ghcpkgs = [p | p <- Map.elems ipkgs, all || ipkgpath p == "lang/ghc"]
  putStr $ unlines $ map show ghcpkgs

processFile ::
  InstalledPackages ->
  PkgMap ->
  DB.HackageDB ->
  String ->
  IO ()
processFile ipkgs hpkgs hdb f =
  if ".cabal" `isSuffixOf` f
    then do
      systemPkgs <- Map.keysSet <$> bundledPackages
      dumpCabalDeps systemPkgs f
    else findPkg ipkgs hpkgs hdb f

findPkg ::
  InstalledPackages ->
  PkgMap ->
  DB.HackageDB ->
  String ->
  IO ()
findPkg ipkgs hpkgs hdb p = do
  print p
  let pkgs' = bydistname hpkgs
      pkgName = mkPackageName p
      printJust = printJust' putStrLn
      printJust' printer x f = maybe (pure ()) (printer . f) x
  printJust' Text.putStrLn (Map.lookup pkgName pkgs') $ \pkg ->
    "sqlports:\t" <> fullpkgpath pkg <> " (" <> Text.pack (show (distVersion pkg)) <> ")"
  printJust (Map.lookup pkgName ipkgs) $ \pkg ->
    "ghc-pkg:\t" <> ipkgpath pkg
      <> " ("
      <> prettyShow (pkgVersion $ sourcePackageId pkg)
      <> ")"
  printJust (Map.lookup pkgName hdb) $ \pkg ->
    "hackage:\t" <> latestFromPackageHackage pkg

latestFromPackageHackage :: DB.PackageData -> String
latestFromPackageHackage m = fromMaybe "not found" latest
  where
    latest =
      prettyShow . pkgVersion . PD.package . PD.packageDescription
        . DB.cabalFile
        . fst <$> Map.maxView m
