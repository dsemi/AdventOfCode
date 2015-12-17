module Advent.Day10
    ( part1
    , part2
    ) where

import Advent.Problem

import Control.Monad
import Data.List (group)

lookAndSay :: String -> String
lookAndSay = concatMap (liftM2 (++) (show . length) (take 1)) . group

part1 :: Problem
part1 = Pure $ length . (!! 40) . iterate lookAndSay

part2 :: Problem
part2 = Pure $ length . (!! 50) . iterate lookAndSay
