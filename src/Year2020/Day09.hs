module Year2020.Day09
    ( part1
    , part2
    ) where

import Data.List (tails)
import qualified Data.Set as S
import Data.Sequence ((|>))
import qualified Data.Sequence as Q


parseInput :: String -> [Int]
parseInput = map read . lines

findFirstInvalid :: [Int] -> Int
findFirstInvalid ns = fst $ head $ filter (uncurry isSum)
                      $ zip (drop 25 ns) $ map (take 25) $ tails ns
    where isSum n xs = not $ any (\x -> (n - x) `S.member` S.delete x set) xs
              where set = S.fromList xs

findWeakness :: Int -> [Int] -> Int
findWeakness n = go Q.empty
    where go _ [] = error "not found"
          go window (x:xs)
              | sum window == n = minimum window + maximum window
              | sum window + x > n = go (Q.drop 1 window) (x:xs)
              | otherwise = go (window |> x) xs

part1 :: String -> Int
part1 = findFirstInvalid . parseInput

part2 :: String -> Int
part2 (parseInput -> ns) = findWeakness (findFirstInvalid ns) ns
