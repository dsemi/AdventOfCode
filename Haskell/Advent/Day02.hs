module Advent.Day02
    ( part1
    , part2
    ) where

import Advent.Problem

import Data.String.Utils

getLWHs :: [String] -> [[Int]]
getLWHs = map (map read . split "x")

p1 :: String -> Int
p1 input = sum [ 2*l*w + 2*l*h + 2*w*h + minimum [l*w, l*h, w*h]
               | [l, w, h] <- getLWHs $ lines input]

part1 :: Problem
part1 = Pure p1

p2 :: String -> Int
p2 input = sum [ l*w*h + 2 * minimum [l+w, l+h, w+h]
               | [l, w, h] <- getLWHs $ lines input]

part2 :: Problem
part2 = Pure p2
