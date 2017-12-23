module Year2016.Day12
    ( part1
    , part2
    ) where

import Year2016.Assembunny

import Control.Lens

part1 :: String -> Int
part1 = (^?! (regs . ix 'a')) . evaluate . parseInstructions

part2 :: String -> Int
part2 = (^?! (regs . ix 'a')) . evaluate . ((regs . ix 'c') .~ 1) . parseInstructions
