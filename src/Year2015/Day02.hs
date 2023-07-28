module Year2015.Day02
    ( part1
    , part2
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B

import Scanf

process :: (Int -> Int -> Int -> Int) -> ByteString -> Int
process f = sum . map (f |. scanf [fmt|%dx%dx%d|]) . B.lines

part1 :: ByteString -> Int
part1 = process $ \l w h -> 2*l*w + 2*l*h + 2*w*h + minimum [l*w, l*h, w*h]

part2 :: ByteString -> Int
part2 = process $ \l w h -> l*w*h + 2 * minimum [l+w, l+h, w+h]
