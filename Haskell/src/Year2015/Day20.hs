module Year2015.Day20
    ( part1
    , part2
    ) where

import qualified Data.Set as S
import Math.NumberTheory.ArithmeticFunctions

p1 :: String -> Int
p1 input = fromInteger . head $ dropWhile ((<minPresents) . (*10) . sigma 1) [1..]
    where minPresents = read input

part1 :: String -> String
part1 = show . p1

p2 :: String -> Int
p2 input =  fromInteger . head $ dropWhile (\n -> (<minPresents) . (*11) . S.foldl' (+) 0
                                                  . S.filter ((>=n) . (*50)) $ divisors n) [1..]
    where minPresents = read input

part2 :: String -> String
part2 = show . p2
