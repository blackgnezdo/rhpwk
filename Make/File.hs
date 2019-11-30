{-# LANGUAGE OverloadedStrings #-}

-- | Makefile parse/render.
module Make.File
  ( fragments,
    pruneFrags,
    updateFile,
    updateText,
    runsWhile,
  )
where

import Data.Bifunctor (first)
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Text.PrettyPrint hiding ((<>))

type DepFragment = (String, [(String, [String])])

updateText :: [DepFragment] -> Text -> Text
updateText frags = mconcat . fmap edit . fragments
  where
      replacements =
        [ (Text.pack $ fst frag, Text.pack . render $ renderFrag frag)
          | frag <- frags
        ]
      edit frag = foldr replaceIfMatches frag replacements
      replaceIfMatches (name, replacement) frag
        | name `Text.isPrefixOf` frag = replacement <> "\n"
        | otherwise = frag

updateFile :: FilePath -> [DepFragment] -> IO ()
updateFile name frags = do
  updateText frags <$> Text.readFile name >>= Text.writeFile name
  putStrLn $ "Appended to " <> name

-- | Splits the given text into fragments spanning lines with
-- continuations indicated by a trailing backslash.
fragments :: Text -> [Text]
fragments ts = Text.unlines <$> runsWhile hasContinuation (Text.lines ts)
  where hasContinuation = Text.isSuffixOf "\n"

-- | Produces runs of elements that match the given predicate until it
-- fails to hold (e.g. continuation lines indicated by trailing \\).
runsWhile :: (a -> Bool) -> [a] -> [[a]]
runsWhile p as = go id as
  where
    go acc [] = []
    go acc [a] = [acc [a]]
    go acc (a : as)
      | p a = go (acc . (a :)) as
      | otherwise = acc [a] : go id as

renderFrag :: DepFragment -> Doc
renderFrag (var, ds) =
  vcat $ punctuate " \\" $
    zipWith
      (<>)
      (text var <+> "+=\t\t" : repeat "\t\t\t")
      (dep <$> sort ds)
  where
    dep (n, bounds) = hcat $ text n : punctuate comma (text <$> bounds)

pruneFrags :: (String -> Maybe String) -> [DepFragment] -> [DepFragment]
pruneFrags packetize frags =
  [ (what, deps)
    | (what, ps) <- frags,
      let deps = [(hp, rs) | (Just hp, rs) <- first packetize <$> ps],
      not $ null deps
  ]
