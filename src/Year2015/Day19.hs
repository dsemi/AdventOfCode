{-# LANGUAGE OverloadedStrings #-}

module Year2015.Day19
    ( part1
    , part2
    ) where

import Data.Char
import qualified Data.HashSet as S
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec

parseMapping :: Text -> (Text, Text)
parseMapping = fromJust . parseMaybe parser
    where parser :: Parsec () Text (Text, Text)
          parser = (,) <$> takeWhile1P Nothing isAlphaNum <* chunk " => " <*> takeWhile1P Nothing isAlphaNum

singleReplacements :: Text -> Text -> Text -> [Text]
singleReplacements src k v =
    [ T.concat [a, v, T.drop (T.length k) b] | (a, b) <- T.breakOnAll k src ]

part1 :: Text -> Int
part1 input = let (s:_:mappings) = reverse $ T.lines input
                  reps = map parseMapping mappings
              in S.size $ S.fromList $ concatMap (uncurry $ singleReplacements s) reps

part2 :: Text -> Int
part2 input = uppers - (T.count "Rn" mol + T.count "Ar" mol) - 2*T.count "Y" mol - 1
    where mol = head $ reverse $ T.lines input
          uppers = T.length $ T.filter isUpper mol
