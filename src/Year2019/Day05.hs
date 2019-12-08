module Year2019.Day05
    ( part1
    , part2
    ) where

import Year2019.IntCode


part1 :: String -> Int
part1 = last . runV2 [1] . parse

part2 :: String -> Int
part2 = last . runV2 [5] . parse
