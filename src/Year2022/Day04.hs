module Year2022.Day04
    ( part1
    , part2
    ) where

import Data.Maybe
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)

solve :: ((Int, Int, Int, Int) -> Bool) -> String -> Int
solve f = length . filter (f . fromJust . parseMaybe @() p) . lines
    where p = (,,,) <$> decimal <* char '-' <*> decimal <* char ',' <*> decimal <* char '-' <*> decimal

part1 :: String -> Int
part1 = solve $ \(a0, a1, b0, b1) -> a0 <= b0 && a1 >= b1 || b0 <= a0 && b1 >= a1

part2 :: String -> Int
part2 = solve $ \(a0, a1, b0, b1) -> a0 <= b1 && b0 <= a1
