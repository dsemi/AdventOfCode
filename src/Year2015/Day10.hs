module Year2015.Day10
    ( part1
    , part2
    ) where

import Control.Monad
import Data.List (group)


lookAndSay :: String -> String
lookAndSay = concatMap (liftM2 (++) (show . length) (take 1)) . group

part1 :: String -> Int
part1 =  length . (!! 40) . iterate lookAndSay

part2 :: String -> Int
part2 = length . (!! 50) . iterate lookAndSay
