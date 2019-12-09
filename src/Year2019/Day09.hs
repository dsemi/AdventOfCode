module Year2019.Day09
    ( part1
    , part2
    ) where

import Year2019.IntCode


part1 :: String -> Int
part1 = head . runWithInput [1] . parse

part2 :: String -> Int
part2 = head . runWithInput [2] . parse
