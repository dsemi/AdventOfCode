{-# LANGUAGE DataKinds #-}

module Year2020.Day25
    ( part1
    , part2
    ) where

import Utils

import Data.Mod


part1 :: String -> Mod 20201227
part1 input = go 7 card door
    where [card, door] = findAllInts input
          go n cardn doorn
              | n == card = doorn
              | n == door = cardn
              | otherwise = go (n * 7) (cardn * card) (doorn * door)

part2 :: String -> String
part2 = const ""
