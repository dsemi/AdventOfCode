{-# LANGUAGE QuasiQuotes #-}

module Advent.Day25
    ( part1
    , part2
    ) where

import Advent.Problem

import Math.NumberTheory.Powers
import Text.Regex.PCRE.Heavy

parseCoord input = let [r, c] = map (read . fst) $ scan regex input
                   in (r, c)
    where regex = [re|\d+|]

makeCode :: Integer -> Int
makeCode n = fromInteger $ powerMod 252533 n 33554393 * 20151125 `mod` 33554393

part1 :: Problem
part1 = Pure f
    where f input = makeCode index
              where (r, c) = parseCoord input
                    n = r + c - 1
                    index = n * (n - 1) `div` 2 + c - 1

part2 :: Problem
part2 = PureS $ const ""
