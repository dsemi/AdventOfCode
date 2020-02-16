{-# LANGUAGE DataKinds #-}

module Year2015.Day25
    ( part1
    , part2
    ) where

import Data.Mod

import Utils


parseCoord :: [Int] -> (Int, Int)
parseCoord [r, c] = (r, c)
parseCoord _ = error "Too many ints found"

part1 :: String -> Mod 33554393
part1 input = 252533 ^ index * 20151125
    where (r, c) = parseCoord $ findAllInts input
          n = r + c - 1
          index = n * (n - 1) `div` 2 + c - 1

part2 :: String -> String
part2 = const ""
