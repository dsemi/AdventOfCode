module Year2022.Day01
    ( part1
    , part2
    ) where

import Data.List (sortBy)
import Data.List.Split

elves :: String -> [Int]
elves = map (sum . map read . lines) . splitOn "\n\n"

part1 :: String -> Int
part1 = maximum . elves

part2 :: String -> Int
part2 = sum . take 3 . sortBy (flip compare) . elves
