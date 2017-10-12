{-# LANGUAGE OverloadedStrings #-}

module Year2015.Day12
    ( part1
    , part2
    ) where

import Control.Lens (cosmosOf, elemOf, filtered, folded, plate, sumOf, (^?!))
import Data.Aeson (Value)
import Data.Aeson.Lens (_Number, _Object, _String, _Value)


sumNumbers :: Value -> Int
sumNumbers = sumNumbersMatching (const True)

sumNumbersMatching :: (Value -> Bool) -> Value -> Int
sumNumbersMatching f = truncate . sumOf (cosmosOf (plate . filtered f) . _Number)

doesntHaveRed :: Value -> Bool
doesntHaveRed = not . elemOf (_Object . folded . _String) "red"

part1 :: String -> Int
part1 = sumNumbers . (^?! _Value)

part2 :: String -> Int
part2 = sumNumbersMatching doesntHaveRed . (^?! _Value)
