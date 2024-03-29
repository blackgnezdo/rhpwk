{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text, isSuffixOf)
import Data.Text.Arbitrary ()
import Make.File
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

roundTripText :: Text -> Property
roundTripText t = withEol === mconcat (fragments withEol)
  where
    withEol = if "\n" `isSuffixOf` t then t else t <> "\n"

continuationsCollected :: Text -> Bool
continuationsCollected t =
  not $ any ("\\\n" `isSuffixOf`) $ fragments $ t <> "\nfoo\n"

roundTripRuns :: [Bool] -> Property
roundTripRuns bs =
  let runs = runsWhile id bs
      lasts = last <$> runs
      terminators = null lasts || not (or (init lasts))
   in bs === mconcat runs .&&. True === terminators

properties :: TestTree
properties = testGroup "roundTrips" [qcProps]

qcProps =
  testGroup
    "(checked by QuickCheck)"
    [ QC.testProperty "Bools" roundTripRuns,
      QC.testProperty "Text" roundTripText,
      QC.testProperty "Continuation lines collected" continuationsCollected
    ]

unitTests =
  testGroup
    "Unit tests"
    [ testCase "Trivial subtitution" $
        updateText [("FOO", [("bar", ["baz", "foo"])])] "X\nFOO += x\nY"
          @?= "X\nFOO +=\t\tbarbaz,foo\nY\n",
      testCase "Exact subtitution" $
        updateText
          [ ("FOO", [("bar", ["baz", "foo"])]),
            ("FO", [("1", ["2", "3"])])
          ]
          "X\nFOO += x\nY"
          @?= "X\nFOO +=\t\tbarbaz,foo\nY\n"
    ]

main = defaultMain (testGroup "Tests" [properties, unitTests])
