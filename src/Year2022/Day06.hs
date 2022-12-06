module Year2022.Day06
    ( part1
    , part2
    ) where

import Data.List (nub)

solve :: Int -> String -> Int
solve nchars = go nchars
    where go c xs
              | length (nub (take nchars xs)) == nchars = c
              | otherwise = go (c+1) $ tail xs

part1 :: String -> Int
part1 = solve 4

part2 :: String -> Int
part2 = solve 14
