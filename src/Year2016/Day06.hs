module Year2016.Day06
    ( part1
    , part2
    ) where

import Control.Arrow ((&&&))
import Data.List (group, sort, transpose)

count :: String -> [(Int, Char)]
count = sort . map (length &&& head) . group . sort

part1 :: String -> String
part1 = map (snd . last . count) . transpose . lines

part2 :: String -> String
part2 = map (snd . head . count) . transpose . lines
