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

import Database.Sqlports
import Database.GhcPkg
import Data.Map hiding (map)
import Distribution.InstalledPackageInfo
import Distribution.Package
import Prelude hiding (lookup)
import System.Console.GetOpt
import System.Environment

data Flag = All | Dump | Pkgs deriving (Eq, Show)

options :: [OptDescr Flag]
options =
	[ Option ['a']	[]	(NoArg All)		"for -d, dump all package data"
	, Option ['d']	[]	(NoArg Dump)		"dump package data"
	, Option ['p']	[]	(NoArg Pkgs)		"dump installed package data"
	]

usage = usageInfo "Usage: rhpwk [-d [-a]] | [ -p ] | pname ..." options

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
		([], False, False, True)	->	dumpPkgs
		(fs@(_:_), False, False, False) ->	mapM_ findPkg fs
		_	      ->	ioError (userError usage)

dumpWith f = do
	c <- open
	ps <- f c
	close c
	putStr $ unlines $ map show $ elems ps

dumpPkgs = do
	ipkgs <- installedpkgs
	let ipkgpath = Distribution.InstalledPackageInfo.pkgpath
	let unPkgName (PackageName n) = n
	let ghcpkgs = [ unPkgName $ pkgName $ sourcePackageId p | p <- elems ipkgs, ipkgpath p == "lang/ghc" ]
	print ghcpkgs

findPkg p = do
	c <- open
	pkgs <- hspkgs c
	close c
	let pkgs' = bydistname $ elems pkgs
	case lookup p pkgs' of
		Just pkg ->	print pkg
		Nothing  ->	return ()
	ipkgs <- installedpkgs
	case lookup p ipkgs of
		Just pkg ->	print pkg
		Nothing  ->	return ()
