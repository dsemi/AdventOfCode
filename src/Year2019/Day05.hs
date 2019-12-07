module Year2019.Day05
    ( part1
    , part2
    ) where

import Control.Lens

import Year2019.IntCode


part1 :: String -> Int
part1 = last . run . (input .~ [1]) . parse

part2 :: String -> Int
part2 = last . run . (input .~ [5]) . parse
