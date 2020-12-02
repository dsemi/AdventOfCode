module Year2020.Day01
    ( part1
    , part2
    ) where

import Utils


solve :: Int -> String -> Int
solve n = product . head . filter ((==2020) . sum) . flip combinations n . map read . lines

part1 :: String -> Int
part1 = solve 2

part2 :: String -> Int
part2 = solve 3
