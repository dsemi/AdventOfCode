{-# LANGUAGE OverloadedStrings #-}

module Year2015.Day12
    ( part1
    , part2
    ) where

import Control.Lens (cosmosOf, elemOf, filtered, folded, plate, sumOf, (^?!))
import Data.Aeson (Value)
import Data.Aeson.Lens (_Number, _Object, _String, _Value)


sumNumbers :: (Value -> Bool) -> Value -> Int
sumNumbers f = truncate . sumOf (cosmosOf (plate . filtered f) . _Number)

part1 :: String -> Int
part1 = sumNumbers (const True) . (^?! _Value)

part2 :: String -> Int
part2 = sumNumbers (not . elemOf (_Object . folded . _String) "red") . (^?! _Value)
