module Year2022.Day02
    ( part1
    , part2
    ) where

import Data.Char

solve :: (Int -> Int -> Int) -> String -> Int
solve f = sum . map play . lines
    where play [p1, _, p2] = f (ord p1 - ord 'A') (ord p2 - ord 'X')
          play _ = error "Malformed input"

part1 :: String -> Int
part1 = solve $ \a b -> (b - a + 1) `mod` 3 * 3 + b + 1

part2 :: String -> Int
part2 = solve $ \a b -> (a + b - 1) `mod` 3 + 3*b + 1
