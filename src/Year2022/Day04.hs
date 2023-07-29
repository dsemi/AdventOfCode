module Year2022.Day04
    ( part1
    , part2
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B

import Scanf

solve :: (Int -> Int -> Int -> Int -> Bool) -> ByteString -> Int
solve f = length . filter (apply f . scanf [fmt|%d-%d,%d-%d|]) . B.lines

part1 :: ByteString -> Int
part1 = solve $ \a0 a1 b0 b1 -> a0 <= b0 && a1 >= b1 || b0 <= a0 && b1 >= a1

part2 :: ByteString -> Int
part2 = solve $ \a0 a1 b0 b1 -> a0 <= b1 && b0 <= a1
