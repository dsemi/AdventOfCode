module Year2019.Day05
    ( part1
    , part2
    ) where

import DaysTH
import Year2019.IntCode


$(buildProb)

part1' :: String -> Int
part1' = runV2 1 . parse

part2' :: String -> Int
part2' = runV2 5 . parse
