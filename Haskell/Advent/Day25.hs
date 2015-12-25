{-# LANGUAGE QuasiQuotes, BangPatterns #-}

module Advent.Day25
    ( part1
    , part2
    ) where

import Advent.Problem

import Text.Regex.PCRE.Heavy

parseCoord input = let [r, c] = map (read . fst) $ scan regex input
                   in (r, c)
    where regex = [re|\d+|]

makeCode = mc 20151125
    where mc !c 0 = c
          mc !c !n = mc (c * 252533 `rem` 33554393) $ n - 1

part1 :: Problem
part1 = Pure f
    where f input = makeCode index
              where (r, c) = parseCoord input
                    index = sum [1..r+c-2] + c - 1

part2 :: Problem
part2 = PureS $ const ""
