module Year2016.Day23
    ( part1
    , part2
    ) where

import Control.Lens
import Data.ByteString (ByteString)

import Year2016.Assembunny

part1 :: ByteString -> Int
part1 = (^. a) . evaluate . (a .~ 7) . parseInstructions

part2 :: ByteString -> Int
part2 = (^. a) . evaluate . (a .~ 12) . parseInstructions
