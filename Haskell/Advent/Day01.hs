module Advent.Day01
    ( part1
    , part2
    ) where

import Advent.Problem

import Data.List (findIndex)
import Data.Maybe

floorDiff :: Char -> Int
floorDiff '(' =  1
floorDiff ')' = -1
floorDiff  _  = undefined

part1 :: Problem
part1 = Pure $ sum . map floorDiff

part2 :: Problem
part2 = Pure $ (+1) . fromJust . findIndex (<0) . calcFloors
    where calcFloors = scanl1 (+) . map floorDiff
