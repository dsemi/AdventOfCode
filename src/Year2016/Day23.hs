module Year2016.Day23
    ( part1
    , part2
    ) where

import Year2016.Assembunny

import Control.Lens (set, view)

part1 :: String -> Int
part1 = view a . evaluate . set a 7 . parseInstructions

part2 :: String -> Int
part2 = view a . evaluate . set a 12 . parseInstructions
