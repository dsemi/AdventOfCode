module Year2015.Day17
    ( part1
    , part2
    ) where

import Data.List (sortBy)

combos :: Int -> Int -> [Int] -> [Int]
combos x y input = go (sortBy (flip compare) input) x y
    where go containers nog len
             | nog == 0 = [len]
             | otherwise = case containers' of
                             [] -> []
                             (n:rest) -> go rest nog len ++ go rest (nog - n) (len + 1)
             where containers' = dropWhile (>nog) containers

part1 :: String -> Int
part1 = length . combos 150 0 . map read . lines

part2 :: String -> Int
part2 input = let lens = combos 150 0 $ map read $ lines input
                  m = minimum lens
              in length $ filter (== m) lens
