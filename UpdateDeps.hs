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
import qualified Data.Map as Map
import qualified Data.Text as Text
import Database.GhcPkg (bundledPackages)
import Database.Sqlports
  ( GPkg (..),
    bydistname,
    close,
    hackageVersion,
    hspkgs,
    open,
  )
import qualified Distribution.Hackage.DB as DB
import Distribution.Package (mkPackageName, unPackageName)
import qualified Distribution.PackageDescription as PD
import Distribution.Types.Version (mkVersion')
import Make.File (pruneFrags, updateFile)
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
  pkgs <- bydistname <$> bracket open close hspkgs
  hdb <- DB.hackageTarball >>= DB.readTarball Nothing
  systemPkgs <- Map.keysSet <$> bundledPackages
  let packetizeName p = Text.unpack . fullpkgpath <$> Map.lookup p' pkgs
        where
          p' = mkPackageName p
      crossCheck = Map.intersectionWith (\p d -> (p, lookupDescription p d)) pkgs hdb
  forM_ (Map.assocs crossCheck) $ \(pname, (hpkg, mbPkg)) -> do
    putStrLn $
      Text.unpack (fullpkgpath hpkg)
        <> " "
        <> unPackageName pname
    case mbPkg of
      Left err -> putStrLn err
      Right pkg -> do
        case dumpDepsFromPD systemPkgs pkg of
          [] -> putStrLn "Nothing to do"
          frags ->
            let name = "/usr/ports" </> Text.unpack (pkgpath hpkg) </> "Makefile"
             in updateFile name (pruneFrags packetizeName frags)
