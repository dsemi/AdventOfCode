module Year2015.Day20
    ( part1
    , part2
    ) where

import qualified Data.Set as S
import Math.NumberTheory.ArithmeticFunctions


part1 :: String -> Int
part1 input = fromInteger . head $ dropWhile ((<minPresents) . (*10) . sigma 1) [1..]
    where minPresents = read input

part2 :: String -> Int
part2 input = fromInteger . head $ dropWhile (\n -> (<minPresents) . (*11) . S.foldl' (+) 0
                                                    . S.filter ((>=n) . (*50)) $ divisors n) [1..]
    where minPresents = read input
