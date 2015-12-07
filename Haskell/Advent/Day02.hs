module Advent.Day02
    ( part1
    , part2
    ) where

import Data.String.Utils

getLWHs :: [String] -> [[Int]]
getLWHs = map (map read . split "x")

part1 :: String -> String
part1 input = show $ sum [ 2*l*w + 2*l*h + 2*w*h + minimum [l*w, l*h, w*h]
                         | [l, w, h] <- getLWHs $ lines input]

part2 :: String -> String
part2 input = show $ sum [ l*w*h + 2 * minimum [l+w, l+h, w+h]
                         | [l, w, h] <- getLWHs $ lines input]
