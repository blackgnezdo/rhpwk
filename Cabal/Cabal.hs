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

module Cabal.Cabal (
    dumpCabalDeps
  , dumpDepsFromPD
  , refineDescription
) where

import Control.Monad (unless)
import Data.Maybe (fromJust, isJust)
import Data.Set (Set)
import qualified Data.Set as Set
import Distribution.Compiler (AbiTag(..), buildCompilerId, unknownCompilerInfo)
import Distribution.Package (Dependency(..))
import Distribution.PackageDescription
  ( BuildInfo(..)
  , GenericPackageDescription
  , PackageDescription(..)
  , allBuildInfo
  , buildInfo
  , libBuildInfo
  )
import Distribution.Package (PackageName)
import Distribution.PackageDescription.Configuration (finalizePD)
import Distribution.PackageDescription.Parsec (readGenericPackageDescription)
import Distribution.Pretty (prettyShow)
import Distribution.Simple.BuildToolDepends (getAllToolDependencies)
import Distribution.System (buildPlatform)
import Distribution.Types.ComponentRequestedSpec (defaultComponentRequestedSpec)
import Distribution.Types.Dependency (depPkgName)
import Distribution.Types.ExeDependency (ExeDependency(..))
import Distribution.Types.PackageName (unPackageName)
import Distribution.Verbosity (silent)
import Distribution.Version
  ( Bound(..)
  , LowerBound(..)
  , UpperBound(..)
  , VersionInterval
  , VersionRange
  , asVersionIntervals
  , version0
  )

refineDescription :: GenericPackageDescription -> Either [Dependency] PackageDescription
refineDescription gp =
  let spec = defaultComponentRequestedSpec
      checkDep = const True
      plat = buildPlatform
      compiler = unknownCompilerInfo buildCompilerId NoAbiTag
      extraDeps = []
  in fst <$> finalizePD mempty spec checkDep plat compiler extraDeps gp

dumpCabalDeps :: Set PackageName -> FilePath -> IO ()
dumpCabalDeps systemPkgs f = do
  putStrLn $ " --- " <> f
  descr <- refineDescription <$> readGenericPackageDescription silent f
  dumpDepsFromPD systemPkgs $ either (error . show) id descr

dumpDepsFromPD :: Set PackageName -> PackageDescription -> IO ()
dumpDepsFromPD systemPkgs p = do
  let lib = library p
      execs = executables p
      hasLib = isJust lib
      hasExecs = not $ null $ execs
      libDeps = targetBuildDepends $ libBuildInfo $ fromJust lib
      execDeps = concatMap (targetBuildDepends . buildInfo) execs
      allBuilds = allBuildInfo p :: [BuildInfo]
      buildDeps = concatMap (getAllToolDependencies p) allBuilds
      pared ds = [d | d <- ds, not $ depPkgName d `Set.member` systemPkgs]
  unless (null buildDeps) $
          printExeDeps "BUILD_DEPENDS" buildDeps
  if (hasLib && hasExecs) then do
          printDeps "RUN_DEPENDS-lib" (pared libDeps)
          printDeps "RUN_DEPENDS-main" (pared execDeps)
  else if hasLib then
          printDeps "RUN_DEPENDS" (pared libDeps)
  else if hasExecs then
          printDeps "RUN_DEPENDS" (pared execDeps)
  else
          return ()

printExeDeps :: String -> [ExeDependency] -> IO ()
printExeDeps what ds = do
	print what
	mapM_ print $ map exeDep ds

exeDep :: ExeDependency -> (String, [String])
exeDep (ExeDependency pkg _ vs) = (unPackageName pkg, printVR vs)

printVR :: VersionRange -> [String]
printVR = map printVI . asVersionIntervals

printDeps :: String -> [Dependency] -> IO ()
printDeps what ds = do
	print what
	mapM_ print $ map printDep ds

printDep :: Dependency -> (String, [String])
printDep (Dependency pkg vr) =
	(unPackageName pkg, map printVI $ asVersionIntervals vr)

printVI :: VersionInterval -> String
printVI (LowerBound lv InclusiveBound, NoUpperBound) | lv == version0 = ""
printVI (LowerBound lv lb, NoUpperBound) = printB '>' lb ++ prettyShow  lv
printVI (LowerBound lv InclusiveBound, UpperBound uv InclusiveBound)
	| lv == uv = '=' : prettyShow lv
printVI (LowerBound lv lb, UpperBound uv ub) =
	printB '>' lb ++ prettyShow lv ++ "," ++ printB '<' ub ++ prettyShow uv

printB :: Char -> Bound -> String
printB op InclusiveBound = op : "="
printB op ExclusiveBound = [op]
 
