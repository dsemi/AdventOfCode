module Year2022.Day02
    ( part1
    , part2
    ) where

import Data.Char

beat :: Int -> Int
beat = (`mod` 3) . succ

lose :: Int -> Int
lose = (`mod` 3) . pred

solve :: (Char -> Int -> Int) -> String -> Int
solve conv = sum . map play . lines
    where play [p1, _, p2] =
              let a = ord p1 - ord 'A'
                  b = conv p2 a
                  wld | b == beat a = 6
                      | b == lose a = 0
                      | otherwise = 3
              in wld + b + 1
          play _ = error "Malformed input"

part1 :: String -> Int
part1 = solve $ \case
                'X' -> const 0
                'Y' -> const 1
                'Z' -> const 2

part2 :: String -> Int
part2 = solve $ \case
                'X' -> lose
                'Y' -> id
                'Z' -> beat
