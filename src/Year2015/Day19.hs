{-# LANGUAGE OverloadedStrings #-}

module Year2015.Day19
    ( part1
    , part2
    ) where

import Utils

import Control.Lens
import qualified Data.HashSet as S
import Data.Maybe
import Data.Char
import Data.Text (Text)
import qualified Data.Text as T
import Data.Tuple
import Text.Megaparsec


parseMapping :: Text -> (Text, Text)
parseMapping = fromJust . parseMaybe parser
    where parser :: Parsec () Text (Text, Text)
          parser = (,) <$> someP isAlphaNum <* chunk " => " <*> someP isAlphaNum

singleReplacements :: Text -> Text -> Text -> [Text]
singleReplacements src k v =
    [ T.concat [a, v, T.drop (T.length k) b] | (a, b) <- T.breakOnAll k src ]

part1 :: Text -> Int
part1 input = let (s:_:mappings) = reverse $ T.lines input
                  reps = map parseMapping mappings
              in S.size $ S.fromList $ concatMap (uncurry $ singleReplacements s) reps

findPathToElectron :: [(Text, Text)] -> Text -> Int
findPathToElectron reps = go 0
    where findMatch = parse (searchAll $ choice $ map (chunk . fst) reps) ""
          rep w = fromJust $ lookup w reps
          go c "e" = c
          go c s = let (Right m) = findMatch s
                   in go (c+1) $ replace1 m (rep m) s

part2 :: Text -> Int
part2 input = let (s:_:mappings) = reverse $ T.lines input
                  reps = map (swap . (over both T.reverse) . parseMapping) mappings
              in findPathToElectron reps $ T.reverse s
