{-# LANGUAGE BangPatterns #-}

module Year2019.Day04
    ( part1
    , part2
    ) where

import Data.ByteString (ByteString)
import Data.Ix (range)

import Scanf

parseRange :: ByteString -> (Int, Int)
parseRange = apply (,) . scanf [fmt|%d-%d|]

numValid :: (Int -> Bool) -> ByteString -> Int
numValid f = length . filter solve . range . parseRange
    where solve num = go (num `rem` 10) 1 False (num `quot` 10)
          go _ !c !b 0 = b || f c
          go prev !c !b n =
              case compare m prev of
                EQ -> go prev (c+1) b n'
                GT -> False
                LT -> go m 1 (b || f c) n'
              where m = n `rem` 10
                    n' = n `quot` 10

part1 :: ByteString -> Int
part1 = numValid (>=2)

part2 :: ByteString -> Int
part2 = numValid (==2)
