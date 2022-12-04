module Year2022.Day04
    ( part1
    , part2
    ) where

import Data.List.Split

solve :: (Int -> Int -> Int -> Int -> Bool) -> String -> Int
solve f = length . filter (check . map (map read . splitOn "-") . splitOn ",") . lines
    where check [[a0, a1], [b0, b1]] = f a0 a1 b0 b1
          check _ = error "Malformed input"

part1 :: String -> Int
part1 = solve $ \a0 a1 b0 b1 -> a0 <= b0 && a1 >= b1 || b0 <= a0 && b1 >= a1

part2 :: String -> Int
part2 = solve $ \a0 a1 b0 b1 -> a0 <= b1 && b0 <= a1
