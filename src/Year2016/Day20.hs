module Year2016.Day20
    ( part1
    , part2
    ) where

import Data.List (sort)
import Data.Maybe
import Text.Megaparsec (parseMaybe, single)
import Text.Megaparsec.Char.Lexer (decimal)


collapseIpFilters :: String -> [(Int, Int)]
collapseIpFilters = go . sort . map parse . lines
    where parse = fromJust . parseMaybe @() ((,) <$> decimal <* single '-' <*> decimal)
          go [] = []
          go [y] = [y]
          go ((low0, high0) : (low1, high1) : rest)
              | low1 <= high0 + 1 = go $ (low0, max high0 high1) : rest
              | otherwise = (low0, high0) : go ((low1, high1) : rest)

part1 :: String -> Int
part1 = (\(a, b) -> if a > 0 then 0 else b+1) . head . collapseIpFilters

part2 :: String -> Int
part2 = (2^32 -) . sum . map ((+1) . abs . uncurry (-)) . collapseIpFilters
