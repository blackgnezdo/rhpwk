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

module Main(main) where

import Database.Sqlports
import Data.Map (elems)
import Data.Maybe (fromMaybe)
import System.Console.GetOpt
import System.Environment

data Flag = All | Dump deriving (Eq, Show)

options :: [OptDescr Flag]
options =
	[ Option ['a']		[]	(NoArg All)	"for -d, dump all package data"
	, Option ['d']		[]	(NoArg Dump)	"dump package data"
	]

usage = usageInfo "Usage: rhpwk [-d [-a]]" options

parseOpts :: [String] -> IO ([Flag], [String])
parseOpts argv =
	case getOpt Permute options argv of
		(o, n, []) ->	return (o, n)
		(_, _, es) ->	ioError (userError usage)


main = do
	argv <- getArgs
	(flags, args) <- parseOpts argv
	case args of
		(_:_) ->	ioError (userError usage)
		_     ->	return ()
	case (Dump `elem` flags, All `elem` flags) of
		(True, False) ->	dumpWith hspkgs
		(True, True)  ->	dumpWith allpkgs
		_	      ->	ioError (userError usage)

dumpWith f = do
	c <- open
	ps <- f c
	close c
	putStr $ unlines $ map show $ elems ps
