module Year2020.Day06
    ( part1
    , part2
    ) where

import Data.List (intersect, union)
import Data.List.Split


solve :: (String -> String -> String) -> String -> Int
solve f = sum . map (length . foldr1 f . words) . splitOn "\n\n"

part1 :: String -> Int
part1 = solve union

part2 :: String -> Int
part2 = solve intersect
