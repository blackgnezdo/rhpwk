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
) where

import Control.Monad
import Data.Maybe
import Data.Version hiding (showVersion)
import Distribution.Compiler
import Distribution.Package
import Distribution.Types.PackageName
import Distribution.PackageDescription.Configuration (finalizePD)
import Distribution.PackageDescription
import Distribution.PackageDescription.Parsec (readGenericPackageDescription)
import Distribution.Types.ComponentRequestedSpec (defaultComponentRequestedSpec)
import Distribution.System
import Distribution.Verbosity
import Distribution.Version
import Distribution.Types.ExeDependency
import Distribution.Simple.BuildToolDepends


import System.FilePath

dumpCabalDeps :: FilePath -> IO()
dumpCabalDeps f = do
	gp <- readGenericPackageDescription silent f
	-- Stupid API alert!
	let flags = mempty  -- No special flags
            spec = defaultComponentRequestedSpec
	    checkDep = const True
	    plat = buildPlatform
	    compiler = unknownCompilerInfo buildCompilerId NoAbiTag
	    extraDeps = []
            -- Either [Dependency] (PackageDescription, FlagAssignment)
	    (Right (p, flags')) = finalizePD flags spec checkDep plat compiler extraDeps gp
	    lib = library p
	    execs = executables p
	    hasLib = isJust lib
	    hasExecs = not $ null $ execs
	    libDeps = targetBuildDepends $ libBuildInfo $ fromJust lib
	    execDeps = concatMap (targetBuildDepends . buildInfo) execs
            allBuilds :: [BuildInfo]
            allBuilds = allBuildInfo p
            buildDeps :: [ExeDependency]
	    buildDeps = concatMap (getAllToolDependencies p) allBuilds
	unless (null buildDeps) $
		printExeDeps "BUILD_DEPENDS" buildDeps
	if (hasLib && hasExecs) then do
		printDeps "RUN_DEPENDS-lib" libDeps
		printDeps "RUN_DEPENDS-main"  execDeps
	else if hasLib then
		printDeps "RUN_DEPENDS" libDeps
	else if hasExecs then
		printDeps "RUN_DEPENDS" execDeps
	else
		return ()

printExeDeps :: String -> [ExeDependency] -> IO ()
printExeDeps what ds = do
	print what
	mapM_ print $ map show ds
  
printDeps :: String -> [Dependency] -> IO ()
printDeps what ds = do
	print what
	mapM_ print $ map printDep ds

printDep :: Dependency -> (String, [String])
printDep (Dependency pkg vr) =
	(unPackageName pkg, map printVI $ asVersionIntervals vr)

printVI :: VersionInterval -> String
printVI (LowerBound lv InclusiveBound, NoUpperBound) | lv == mkVersion [0] = ""
printVI (LowerBound lv lb, NoUpperBound) = printB '>' lb ++ showVersion lv
printVI (LowerBound lv InclusiveBound, UpperBound uv InclusiveBound)
	| lv == uv = '=' : showVersion lv
printVI (LowerBound lv lb, UpperBound uv ub) =
	printB '>' lb ++ showVersion lv ++ "," ++ printB '<' ub ++ showVersion uv

printB :: Char -> Bound -> String
printB op InclusiveBound = op : "="
printB op ExclusiveBound = [op]
