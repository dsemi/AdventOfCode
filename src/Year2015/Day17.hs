module Year2015.Day17
    ( part1
    , part2
    ) where

import Utils


allCombinationsTotaling :: Int -> [Int] -> [[[Int]]]
allCombinationsTotaling n xs = map (filter ((==n) . sum) . combinations xs) [1..length xs]

part1 :: String -> Int
part1 = sum . map length . allCombinationsTotaling 150 . map read . lines

part2 :: String -> Int
part2 = length . head . filter (not . null) . allCombinationsTotaling 150 . map read . lines
