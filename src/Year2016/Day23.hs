module Year2016.Day23
    ( part1
    , part2
    ) where

import Year2016.Assembunny

import Control.Lens

part1 :: String -> Int
part1 = (^?! (regs . ix 'a')) . evaluate . ((regs . ix 'a') .~ 7) . parseInstructions

part2 :: String -> Int
part2 = (^?! (regs . ix 'a')) . evaluate . ((regs . ix 'a') .~ 12) . parseInstructions
