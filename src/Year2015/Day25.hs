module Year2015.Day25
    ( part1
    , part2
    ) where

import Utils

import Math.NumberTheory.Powers


parseCoord input = let (Right [r, c]) = findAllInts input
                   in (r, c)

makeCode :: Integer -> Int
makeCode n = fromInteger $ powerMod 252533 n 33554393 * 20151125 `mod` 33554393

part1 :: String -> Int
part1 input = makeCode index
    where (r, c) = parseCoord input
          n = r + c - 1
          index = n * (n - 1) `div` 2 + c - 1

part2 :: String -> String
part2 = const ""
