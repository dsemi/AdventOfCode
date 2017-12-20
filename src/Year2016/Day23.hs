module Year2016.Day23
    ( part1
    , part2
    ) where

import Year2016.Assembunny

import Control.Lens
import Data.Maybe

part1 :: String -> Int
part1 = fromJust . view (regs . at 'a') . evaluate . ((regs . at 'a') ?~ 7) . parseInstructions

part2 :: String -> Int
part2 = fromJust . view (regs . at 'a') . evaluate . ((regs . at 'a') ?~ 12) . parseInstructions
