module Year2020.Day09
    ( part1
    , part2
    ) where

import Utils

import Data.List (tails)
import Data.Vector ((!), fromList, slice)


parseInput :: String -> [Int]
parseInput = map read . lines

findFirstInvalid :: [Int] -> Int
findFirstInvalid ns = fst $ head $ filter (uncurry isSum)
                      $ zip (drop 25 ns) $ map (take 25) $ tails ns
    where isSum n xs = not $ any (== n) $ map sum $ combinations xs 2

findWeakness :: Int -> [Int] -> Int
findWeakness n ns = go 0 0 0
    where v = fromList ns
          go lo hi c
              | c < n = go lo (hi + 1) $ c + v ! hi
              | c > n = go (lo + 1) hi $ c - v ! lo
              | otherwise = let s = slice lo (hi - lo) v
                            in minimum s + maximum s

part1 :: String -> Int
part1 = findFirstInvalid . parseInput

part2 :: String -> Int
part2 (parseInput -> ns) = findWeakness (findFirstInvalid ns) ns
