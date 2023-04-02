module Year2020.Day13
    ( part1
    , part2
    ) where

import Data.List (minimumBy)
import Data.List.Split (splitOn)
import Data.Ord (comparing)

import Utils hiding (splitOn)

parse :: String -> (Integer, [(Integer, Integer)])
parse input = (read time, buses)
    where [time, row2] = lines input
          buses = [ (-i, read b) | (i, b) <- zip [0..] $ splitOn "," row2
                  , b /= "x" ]

part1 :: String -> Integer
part1 input = uncurry (*) $ minimumBy (comparing snd)
              $ map ((\b -> (b, b - earliestTime `mod` b)) . snd) buses
    where (earliestTime, buses) = parse input

part2 :: String -> Maybe Integer
part2 input = chineseRemainder buses
    where buses = snd $ parse input
