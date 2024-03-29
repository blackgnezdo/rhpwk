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

module Cabal.Cabal
  ( dumpCabalDeps,
    dumpDepsFromPD,
    refineDescription,
    PkgSpec,
    parsePkgSpec,
  )
where

import Control.Monad (forM_)
import Data.List (nub)
import Data.Maybe (fromJust, isJust)
import Data.Set (Set)
import qualified Data.Set as Set
import Distribution.Compiler (AbiTag (..), buildCompilerId, unknownCompilerInfo)
import Distribution.Package (Dependency (..), PackageName)
import Distribution.PackageDescription
  ( BuildInfo (..),
    GenericPackageDescription,
    PackageDescription (..),
    allBuildInfo,
    buildInfo,
    libBuildInfo,
  )
import Distribution.PackageDescription.Configuration (finalizePD)
import Distribution.PackageDescription.Parsec (readGenericPackageDescription)
import Distribution.Parsec.Class (simpleParsec)
import Distribution.Pretty (prettyShow)
import Distribution.Simple.BuildToolDepends (getAllToolDependencies)
import Distribution.System (buildPlatform)
import Distribution.Types.ComponentRequestedSpec (defaultComponentRequestedSpec)
import Distribution.Types.Dependency (depPkgName)
import Distribution.Types.ExeDependency (ExeDependency (..))
import Distribution.Types.PackageName (unPackageName)
import Distribution.Verbosity (silent)
import Distribution.Version
  ( Bound (..),
    LowerBound (..),
    UpperBound (..),
    VersionInterval,
    VersionRange,
    anyVersion,
    asVersionIntervals,
    version0,
  )
import Text.PrettyPrint (text)
import Text.PrettyPrint.GenericPretty (Out (..))

newtype PkgSpec = PkgSpec {unPkgSpec :: VersionRange} deriving (Eq)

instance Show PkgSpec where
  -- OpenBSD pkgSpec only supports a single VersionInterval.
  -- Hence concat below should not produce malformed values.
  showsPrec _ s = (concat (printVR (unPkgSpec s)) <>)

instance Out PkgSpec where

  docPrec _ = doc

  doc = text . show

parsePkgSpec :: String -> Maybe PkgSpec
parsePkgSpec "" = Just $ PkgSpec anyVersion
parsePkgSpec pkgSpecStr = PkgSpec <$> (cleaner pkgSpecStr >>= simpleParsec)
  where
    cleaner :: String -> Maybe String
    -- Special case of exact match starts with "=", the other OpenBSD
    -- package spec operators start with "<" or ">".
    cleaner ('S' : 'T' : 'E' : 'M' : '-' : version@('=' : _)) =
      Just $ '=' : version
    cleaner ('S' : 'T' : 'E' : 'M' : '-' : version) =
      Just $ concatMap replaceComma version
    cleaner _ = Nothing
    -- OpenBSD packages-specs are separated by comma whereas
    -- Cabal's by &&. Not using a true parser here because it
    -- seems like a simple enough case and the data is supposedly
    -- pre-validated by sqlports.
    replaceComma ',' = "&&"
    replaceComma x = [x]

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
  let frags = dumpDepsFromPD systemPkgs $ either (error . show) id descr
  forM_ frags $ \(what, ps) -> do
    putStrLn $ what <> "="
    forM_ ps $ \(p, rs) -> putStrLn $ "\t" <> p <> concat rs

dumpDepsFromPD :: Set PackageName -> PackageDescription -> [(String, [(String, [String])])]
dumpDepsFromPD systemPkgs p =
  let lib = library p
      execs = executables p
      hasLib = isJust lib
      hasExecs = not $ null execs
      libDeps = targetBuildDepends $ libBuildInfo $ fromJust lib
      execDeps = concatMap (targetBuildDepends . buildInfo) execs
      allBuilds = allBuildInfo p :: [BuildInfo]
      buildDeps = concatMap (getAllToolDependencies p) allBuilds
      pared ds = [d | d <- ds, not $ depPkgName d `Set.member` systemPkgs]
   in fmap (fmap nub) $ filter (not . null . snd) $
        concat
          [ [ ("BUILD_DEPENDS", printExeDep <$> buildDeps)
              | not $ null buildDeps
            ],
            [ ( if hasLib then "RUN_DEPENDS-main" else "RUN_DEPENDS",
                printDep <$> pared execDeps
              )
              | hasExecs
            ],
            [ ( if hasExecs then "RUN_DEPENDS-lib" else "RUN_DEPENDS",
                printDep <$> pared libDeps
              )
              | hasLib
            ]
          ]

printExeDep :: ExeDependency -> (String, [String])
printExeDep (ExeDependency pkg _ vs) = (unPackageName pkg, printVR vs)

printVR :: VersionRange -> [String]
printVR = map printVI . asVersionIntervals

printDep :: Dependency -> (String, [String])
printDep (Dependency pkg vr) =
  (unPackageName pkg, map printVI $ asVersionIntervals vr)

printVI :: VersionInterval -> String
printVI (LowerBound lv InclusiveBound, NoUpperBound)
  | lv == version0 = ""
printVI (LowerBound lv lb, NoUpperBound) = printB '>' lb ++ prettyShow lv
printVI (LowerBound lv InclusiveBound, UpperBound uv InclusiveBound)
  | lv == uv = '=' : prettyShow lv
printVI (LowerBound lv InclusiveBound, UpperBound uv ub)
  | lv == version0 = printB '<' ub ++ prettyShow uv
printVI (LowerBound lv lb, UpperBound uv ub) =
  printB '>' lb ++ prettyShow lv ++ "," ++ printB '<' ub ++ prettyShow uv

printB :: Char -> Bound -> String
printB op InclusiveBound = op : "="
printB op ExclusiveBound = [op]
