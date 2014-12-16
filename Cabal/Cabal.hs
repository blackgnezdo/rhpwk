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
import Data.Version
import Distribution.Compiler
import Distribution.Package
import Distribution.PackageDescription.Configuration
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.System
import Distribution.Verbosity
import Distribution.Version
import System.FilePath

dumpCabalDeps :: FilePath -> IO()
dumpCabalDeps f = do
	gp <- readPackageDescription silent f
	-- Stupid API alert!
	let flags = []
	    checkDep = const True
	    plat = buildPlatform
	    -- XXX use the actual compiler version instead of
	    -- hardcoding it here. And why isn't there a buildCompilerId
	    -- constant available?
	    compiler = CompilerId buildCompilerFlavor $ Version [7, 8] []
	    extraDeps = []
	    (Right (p, flags')) = finalizePackageDescription flags checkDep plat compiler extraDeps gp
	    lib = library p
	    execs = executables p
	    hasLib = isJust lib
	    hasExecs = not $ null $ execs
	    libDeps = targetBuildDepends $ libBuildInfo $ fromJust lib
	    execDeps = concatMap (targetBuildDepends . buildInfo) execs
	    buildDeps = concatMap buildTools $ allBuildInfo p
	unless (null buildDeps) $
		printDeps "BUILD_DEPENDS" buildDeps
	if (hasLib && hasExecs) then do
		printDeps "RUN_DEPENDS-lib" libDeps
		printDeps "RUN_DEPENDS-main"  execDeps
	else if hasLib then
		printDeps "RUN_DEPENDS" libDeps
	else if hasExecs then
		printDeps "RUN_DEPENDS" execDeps
	else
		return ()

printDeps :: String -> [Dependency] -> IO ()
printDeps what ds = do
	print what
	mapM_ print $ map printDep ds

printDep :: Dependency -> (String, [String])
printDep (Dependency (PackageName pkg) vr) =
	(pkg, map printVI $ asVersionIntervals vr)

printVI :: VersionInterval -> String
printVI (LowerBound (Version [0] []) InclusiveBound, NoUpperBound) = ""
printVI (LowerBound lv lb, NoUpperBound) = printB '>' lb ++ showVersion lv
printVI (LowerBound lv InclusiveBound, UpperBound uv InclusiveBound)
	| lv == uv = '=' : showVersion lv
printVI (LowerBound lv lb, UpperBound uv ub) =
	printB '>' lb ++ showVersion lv ++ "," ++ printB '<' ub ++ showVersion uv

printB :: Char -> Bound -> String
printB op InclusiveBound = op : "="
printB op ExclusiveBound = [op]
