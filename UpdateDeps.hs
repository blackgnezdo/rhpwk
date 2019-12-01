{-# LANGUAGE OverloadedStrings #-}
-- | Replaces dependency information based on hackage declarations in
-- haskell ports Makefiles.
module UpdateDeps
  ( main,
  )
where

import Cabal.Cabal (dumpDepsFromPD, refineDescription)
import Control.Exception (bracket)
import Control.Monad (forM_)
import Data.Bifunctor (first)
import Data.List (nub)
import Data.Fix (unFix)
import qualified Data.Map as Map
import qualified Data.Text as Text
import Database.GhcPkg (bundledPackages)
import Database.Sqlports
  ( GPkg (..),
    Dependency(..),
    ResolvedPkg,
    allpkgs,
    bydistname,
    fixedDep,
    close,
    hackageVersion,
    nonHsDeps,
    open,
    resolvePkgMap,
  )
import qualified Distribution.Hackage.DB as DB
import Distribution.Package (mkPackageName, unPackageName)
import qualified Distribution.PackageDescription as PD
import Distribution.Types.Version (mkVersion')
import Make.File (DepFragment, pruneFrags, updateFile)
import System.FilePath ((</>))

main = updateDeps

lookupDescription ::
  GPkg a ->
  DB.PackageData ->
  Either String PD.PackageDescription
lookupDescription hpkg pkgData = do
  let val <?> msg = maybe (Left msg) pure val
  hv <-
    hackageVersion hpkg
      <?> ("Invalid pkgname " <> Text.unpack (pkgname hpkg))
  vd <-
    Map.lookup (mkVersion' hv) pkgData
      <?> ("No cabal version data " <> show hv)
  first
    (\ds -> "Dependency not resolved" <> show ds)
    (refineDescription $ DB.cabalFile vd)

updateDeps :: IO ()
updateDeps = do
  pkgs <- bydistname . fmap unFix . resolvePkgMap <$> bracket open close allpkgs
  hdb <- DB.hackageTarball >>= DB.readTarball Nothing
  systemPkgs <- Map.keysSet <$> bundledPackages
  let packetizeName p = Text.unpack . fullpkgpath <$> Map.lookup p' pkgs
        where
          p' = mkPackageName p
      crossCheck =
        Map.intersectionWith (\p d -> (p, lookupDescription p d)) pkgs hdb
  forM_ (Map.assocs crossCheck) $ \(pname, (hpkg, cabalPkg)) -> do
    putStrLn $
      Text.unpack (fullpkgpath hpkg)
        <> " "
        <> unPackageName pname
    case cabalPkg of
      Left err -> putStrLn err
      Right pkg -> do
        case dumpDepsFromPD systemPkgs pkg of
          [] -> putStrLn "Nothing to do"
          frags -> do
            let name = "/usr/ports" </> Text.unpack (pkgpath hpkg) </> "Makefile"
            updateFile name (enrich (nonHsDeps hpkg) $ pruneFrags packetizeName frags)
            putStrLn $ "Updated " <> name

-- TODO: use dependency kind to put the enrichments where they belong,
-- not everywhere.
enrich :: [Dependency ResolvedPkg] -> [DepFragment] -> [DepFragment]
enrich deps frags = [(what, ps <> enrichments) | (what, ps) <- frags]
  where enrichments = nub [(Text.unpack $ fullpkgpath $ fixedDep d,
                            [show $ pkgspec d]) | d <- deps]
