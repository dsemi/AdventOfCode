module Year2016.Day12
    ( part1
    , part2
    ) where

import Year2016.Assembunny

import Control.Lens (set, view)

part1 :: String -> Int
part1 = view a . evaluate . parseInstructions

part2 :: String -> Int
part2 = view a . evaluate . set c 1 . parseInstructions
