{-# LANGUAGE OverloadedStrings #-}

module Advent.Day12
    ( part1
    , part2
    ) where

import Control.Lens
import Data.Aeson
import Data.Aeson.Lens

sumNumbers :: Value -> Int
sumNumbers = truncate . sumOf (deep _Number)

removeReds :: Value -> Value
removeReds v
    | elemOf (_Object . folded . _String) "red" v = Number 0
    | otherwise = v

part1 :: String -> String
part1 = show . sumNumbers . (^?! _Value)

part2 :: String -> String
part2 = show . sumNumbers . transform removeReds . (^?! _Value)
