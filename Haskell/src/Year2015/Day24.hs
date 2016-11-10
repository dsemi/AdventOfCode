module Year2015.Day24
    ( part1
    , part2
    ) where

import Data.List (tails)

combinations :: [a] -> Int -> [[a]]
combinations  _ 0 = [[]]
combinations xs n = [ y:ys | y:xs' <- tails xs
                    , ys <- combinations xs' $ n-1 ]

day24 wts nGroups =
    minimum $ head [ quantumEntanglements
                   | cs <- map (combinations wts) [1..]
                   , let quantumEntanglements = [ product c | c <- cs
                                                , sum c == groupSize ]
                   , not $ null quantumEntanglements
                   ]
    where groupSize = sum wts `div` nGroups

part1 :: String -> String
part1 = show . f
    where f input = day24 wts 3
              where wts = map read $ lines input

part2 :: String -> String
part2 = show . f
    where f input = day24 wts 4
              where wts = map read $ lines input