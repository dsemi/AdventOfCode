module Year2019.Day08
    ( part1
    , part2
    ) where

import Data.List (minimumBy, transpose)
import Data.List.Split (chunksOf)
import Data.Ord (comparing)

import Ocr

part1 :: String -> Int
part1 = solve . minimumBy (comparing $ count '0') . chunksOf 150
    where count n = length . filter (==n)
          solve layer = count '1' layer * count '2' layer

part2 :: String -> String
part2 = parseLetters . draw . map (head . filter (/= '2')) . transpose . chunksOf 150
    where draw = ('\n':) . init . unlines . chunksOf 25 . map (\case '0' -> ' '; _ -> '#')
