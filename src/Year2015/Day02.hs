module Year2015.Day02
    ( part1
    , part2
    ) where

import Data.String.Utils


getLWHs :: [String] -> [[Int]]
getLWHs = map (map read . split "x")

part1 :: String -> Int
part1 input = sum [ 2*l*w + 2*l*h + 2*w*h + minimum [l*w, l*h, w*h]
                  | [l, w, h] <- getLWHs $ lines input]

part2 :: String -> Int
part2 input = sum [ l*w*h + 2 * minimum [l+w, l+h, w+h]
                  | [l, w, h] <- getLWHs $ lines input]
