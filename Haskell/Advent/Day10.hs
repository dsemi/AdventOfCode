module Advent.Day10
    ( part1
    , part2
    ) where

import Control.Monad
import Data.List (group)

lookAndSay :: String -> String
lookAndSay = concatMap (liftM2 (++) (show . length) (take 1)) . group

part1 :: String -> String
part1 = show . length . (!! 40) . iterate lookAndSay

part2 :: String -> String
part2 = show . length . (!! 50) . iterate lookAndSay
