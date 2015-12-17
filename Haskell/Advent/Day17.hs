module Advent.Day17
    ( part1
    , part2
    ) where

import Data.List (tails)

combinations :: [a] -> Int -> [[a]]
combinations  _ 0 = [[]]
combinations xs n = [ y:ys | y:xs' <- tails xs
                    , ys <- combinations xs' $ n-1 ]

allCombinationsTotaling :: Int -> [Int] -> [[[Int]]]
allCombinationsTotaling n xs = map (filter ((==n) . sum) . combinations xs) [1..length xs]

parseInput :: String -> [Int]
parseInput = map read . lines

part1 :: String -> String
part1 = show . sum . map length . allCombinationsTotaling 150 . parseInput

part2 :: String -> String
part2 = show . length . head . filter (not . null) . allCombinationsTotaling 150 . parseInput
