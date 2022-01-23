module Year2021.Day07
    ( part1
    , part2
    ) where

import Data.List (sort)
import Data.List.Split (splitOn)

part1 :: String -> Int
part1 input = sum $ map (abs . subtract med) ns
    where ns = sort $ map read $ splitOn "," input
          med = ns !! (length ns `div` 2)

part2 :: String -> Int
part2 input = min (go floor) (go ceiling)
    where ns = sort $ map read $ splitOn "," input
          avg = fromIntegral (sum ns) / fromIntegral (length ns)
          g n = n * (n + 1) `div` 2
          go f = sum $ map (g . abs . subtract (f avg)) ns
