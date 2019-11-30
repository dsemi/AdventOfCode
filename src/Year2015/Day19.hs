{-# LANGUAGE OverloadedStrings #-}

module Year2015.Day19
    ( part1
    , part2
    ) where

import Utils

import Control.Lens
import qualified Data.HashSet as S
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Tuple
import Text.Megaparsec
import Text.Megaparsec.Char (alphaNumChar, string)


parseMapping :: Text -> (Text, Text)
parseMapping = over both T.pack . fromJust . parseMaybe parser
    where parser :: Parsec () Text (String, String)
          parser = (,) <$> some alphaNumChar <* string " => " <*> some alphaNumChar

-- E.g. singleReplacements "abskaalkjdsaajlkdaa" "aa" "xx" ->
--   ["abskxxlkjdsaajlkdaa", "abskaalkjdsxxjlkdaa", "abskaalkjdsaajlkdxx"]
singleReplacements :: Text -> Text -> Text -> [Text]
singleReplacements src k v = [ T.concat $ p : zipWith T.append reps ps
                             | i <- [0 .. length pieces - 2]
                             , let reps = ix i .~ v $ replicate (length pieces - 1) k]
    where pieces@(p:ps) = T.splitOn k src

part1 :: Text -> Int
part1 input = let (s:_:mappings) = reverse $ T.lines input
                  reps = map parseMapping mappings
              in S.size $ S.fromList $ concatMap (uncurry $ singleReplacements s) reps

findPathToElectron :: [(Text, Text)] -> Text -> Int
findPathToElectron reps = go 0
    where findMatch = parse (searchAll $ choice $ map (string . fst) reps) ""
          rep w = fromJust $ lookup w reps
          go c "e" = c
          go c s = let (Right m) = findMatch s
                   in go (c+1) $ replace1 m (rep m) s

part2 :: Text -> Int
part2 input = let (s:_:mappings) = reverse $ T.lines input
                  reps = map (swap . (over both T.reverse) . parseMapping) mappings
              in findPathToElectron reps $ T.reverse s
