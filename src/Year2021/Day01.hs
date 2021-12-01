module Year2021.Day01
    ( part1
    , part2
    ) where


part1 :: [Int] -> Int
part1 ns = length $ filter id $ zipWith (<) ns $ tail ns

part2 :: [Int] -> Int
part2 ns = length $ filter id $ zipWith (<) ns $ drop 3 ns
