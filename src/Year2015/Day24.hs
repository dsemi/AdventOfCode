module Year2015.Day24
    ( part1
    , part2
    ) where

import Utils


day24 :: [Int] -> Int -> Int
day24 wts nGroups =
    minimum $ head [ quantumEntanglements
                   | cs <- map (combinations wts) [1..]
                   , let quantumEntanglements = [ product c | c <- cs
                                                , sum c == groupSize ]
                   , not $ null quantumEntanglements
                   ]
    where groupSize = sum wts `div` nGroups

part1 :: String -> Int
part1 input = day24 wts 3
    where wts = map read $ lines input

part2 :: String -> Int
part2 input = day24 wts 4
    where wts = map read $ lines input
