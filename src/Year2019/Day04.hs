{-# LANGUAGE BangPatterns #-}

module Year2019.Day04
    ( part1
    , part2
    ) where

import Data.ByteString (ByteString)
import Data.Ix (range)
import FlatParse.Basic

parseRange :: ByteString -> Maybe (Int, Int)
parseRange input = case runParser parser input of
                     OK res _ -> Just res
                     _ -> Nothing
    where parser = (,) <$> anyAsciiDecimalInt <* $(char '-') <*> anyAsciiDecimalInt

numValid :: (Int -> Bool) -> ByteString -> Maybe Int
numValid f = fmap (length . filter solve . range) . parseRange
    where solve num = go (num `rem` 10) 1 False (num `quot` 10)
          go _ !c !b 0 = b || f c
          go prev !c !b n =
              case compare m prev of
                EQ -> go prev (c+1) b n'
                GT -> False
                LT -> go m 1 (b || f c) n'
              where m = n `rem` 10
                    n' = n `quot` 10

part1 :: ByteString -> Maybe Int
part1 = numValid (>=2)

part2 :: ByteString -> Maybe Int
part2 = numValid (==2)
