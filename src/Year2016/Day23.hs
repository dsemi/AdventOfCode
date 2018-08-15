module Year2016.Day23
    ( part1
    , part2
    ) where

import Year2016.Assembunny

import Control.Lens

part1 :: String -> Int
part1 = (^?! a) . evaluate . (a .~ 7) . parseInstructions

part2 :: String -> Int
part2 = (^?! a) . evaluate . (a .~ 12) . parseInstructions
