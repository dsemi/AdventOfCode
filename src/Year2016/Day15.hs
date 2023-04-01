{-# LANGUAGE OverloadedStrings #-}

module Year2016.Day15
    ( part1
    , part2
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B

import Utils

parseDisc :: ByteString -> (Integer, Integer)
parseDisc line =
    case findAllInts line of
      [discNum, modulo, _, pos] -> (-pos - discNum, modulo)
      _ -> error "Parse error"

part1 :: ByteString -> Maybe Integer
part1 input = chineseRemainder discs
    where discs = map parseDisc $ B.lines input

part2 :: ByteString -> Maybe Integer
part2 input = chineseRemainder discs
    where discs = map parseDisc $ B.lines $ B.append input extra
          extra = "\nDisc #7 has 11 positions; at time=0, it is at position 0."
