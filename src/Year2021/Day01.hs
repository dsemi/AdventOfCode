module Year2021.Day01
    ( part1
    , part2
    ) where


part1 :: String -> Int
part1 input = length $ filter id $ zipWith (<) ns $ tail ns
    where ns :: [Int]
          ns = map read $ lines input

part2 :: String -> Int
part2 input = length $ filter id $ zipWith (<) ns' $ tail ns'
    where ns :: [Int]
          ns = map read $ lines input
          ns' = zipWith3 (\a b c -> a + b + c) ns (tail ns) (drop 2 ns)
