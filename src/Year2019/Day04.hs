{-# LANGUAGE TypeApplications #-}

module Year2019.Day04
    ( part1
    , part2
    ) where

import Data.Char (digitToInt)
import Data.Ix (range)
import Data.List (group)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

import DaysTH


$(buildProb)

parseRange :: String -> Maybe (Int, Int)
parseRange = parseMaybe @() ((,) <$> decimal <* char '-' <*> decimal)

numValid :: (Int -> Bool) -> String -> Maybe Int
numValid f = fmap (length . filter cond . range) . parseRange
    where cond num = and (zipWith (<=) digits (tail digits))
                     && any f (map length (group digits))
              where digits = map digitToInt $ show num

part1' :: String -> Maybe Int
part1' = numValid (>=2)

part2' :: String -> Maybe Int
part2' = numValid (==2)
