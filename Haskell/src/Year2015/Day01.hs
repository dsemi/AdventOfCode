module Year2015.Day01
    ( part1
    , part2
    ) where

import Data.List (findIndex)
import Data.Maybe

floorDiff :: Char -> Int
floorDiff '(' =  1
floorDiff ')' = -1
floorDiff  _  = undefined

part1 :: String -> String
part1 = show . sum . map floorDiff

part2 :: String -> String
part2 = show . (+1) . fromJust . findIndex (<0) . calcFloors
    where calcFloors = scanl1 (+) . map floorDiff
