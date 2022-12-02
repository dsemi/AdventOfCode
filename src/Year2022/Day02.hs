module Year2022.Day02
    ( part1
    , part2
    ) where

import Data.Char

solve :: (Int -> Int -> Int) -> String -> Int
solve conv = sum . map play . lines
    where play [p1, _, p2] =
              let a = ord p1 - ord 'A'
                  b = conv (ord p2 - ord 'X') a
                  wld | b == (a + 1) `mod` 3 = 6
                      | b == a = 3
                      | otherwise = 0
              in wld + b + 1
          play _ = error "Malformed input"

part1 :: String -> Int
part1 = solve const

part2 :: String -> Int
part2 = solve $ \c a -> (a + c - 1) `mod` 3
