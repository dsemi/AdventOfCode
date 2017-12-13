module Year2017.Day13
    ( part1
    , part2
    ) where

import Data.List (foldl')
import Data.List.Split (splitOn)


parse :: String -> [(Int, Int)]
parse x = [ (a, 2*b-2) | [a, b] <- map (map read . splitOn ": ") $ lines x ]

part1 :: String -> Int
part1 x = sum [ a * (b + 2) `div` 2 | (a, b) <- parse x
              , a `mod` b == 0 ]

findDelay :: [(Int, Int)] -> Int
findDelay scrs = head $ filter f [0 ..]
    where f c = all (\(a, b) -> (a + c) `mod` b /=0) scrs

part2 :: String -> Int
part2 = findDelay . parse
