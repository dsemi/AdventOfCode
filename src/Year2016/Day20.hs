module Year2016.Day20
    ( part1
    , part2
    ) where

import Data.List (sort)
import Data.List.Split (splitOn)


parseInput :: String -> [(Int, Int)]
parseInput = map ((\[x,y] -> (read x, read y)) . splitOn "-") . lines

ipFilter :: [(Int, Int)] -> [Int]
ipFilter = go (-1) []
    where upperBound = 4294967295
          go x c []
              | x < upperBound = c ++ [x..upperBound]
              | otherwise      = c
          go x c ((low, high) : rest)
              | x+1 < low = go high (c++[x+1..low-1]) rest
              | otherwise = go (max x high) c rest

part1 :: String -> Int
part1 = head . ipFilter . sort . parseInput

part2 :: String -> Int
part2 = length . ipFilter . sort . parseInput
