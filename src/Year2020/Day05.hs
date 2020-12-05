module Year2020.Day05
    ( part1
    , part2
    ) where

import Data.List (sort)
import Numeric (readInt)


seatIds :: String -> [Int]
seatIds = map (fst . head . readInt 2 (`elem` "FBLR") f) . lines
    where f d | d `elem` "BR" = 1
              | d `elem` "FL" = 0
              | otherwise = error "Bad parse"

part1 :: String -> Int
part1 = maximum . seatIds

part2 :: String -> Int
part2 = (+1) . fst . head . findGap . sort . seatIds
    where findGap x = filter (\(a, b) -> a + 2 == b) $ zip x $ tail x
