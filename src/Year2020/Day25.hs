{-# LANGUAGE DataKinds #-}

module Year2020.Day25
    ( part1
    , part2
    ) where

import Data.ByteString (ByteString)
import Data.Mod
import Math.NumberTheory.Moduli.Multiplicative
import Math.NumberTheory.Moduli.Singleton

import Utils

part1 :: ByteString -> Maybe (Mod 20201227)
part1 input = fmap (door ^%) $ do
  cg <- cyclicGroup
  discreteLogarithm cg <$> isPrimitiveRoot cg 7 <*> isMultElement card
    where [card, door] = findAllInts input

part2 :: String -> String
part2 = const " "
