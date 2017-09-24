module Year2015.Day01
    ( part1
    , part2
    ) where

import Data.List (findIndex)
import Data.Maybe


floorDiff :: Char -> Int
floorDiff '(' =  1
floorDiff ')' = -1

part1 :: String -> Int
part1 = sum . map floorDiff

part2 :: String -> Int
part2 = (+1) . fromJust . findIndex (<0) . scanl1 (+) . map floorDiff
