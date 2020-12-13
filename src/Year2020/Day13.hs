module Year2020.Day13
    ( part1
    , part2
    ) where

import Data.List (minimumBy)
import Data.List.Split (splitOn)
import Data.Ord (comparing)
import Math.NumberTheory.Moduli.Chinese (chineseRemainder)


parse :: String -> (Integer, [(Integer, Integer)])
parse input = (read time, buses)
    where [time, row2] = lines input
          buses = [ (i, read b) | (i, b) <- zip [0..] $ splitOn "," row2
                  , b /= "x" ]

part1 :: String -> Integer
part1 input = busNo * (actualTime - earliestTime)
    where (earliestTime, buses) = parse input
          (busNo, actualTime) = minimumBy (comparing snd)
                                $ map ((\b -> (b, b * (earliestTime `div` b + 1))) . snd) buses

part2 :: String -> Maybe Integer
part2 input = chineseRemainder [ (-i, b) | (i, b) <- buses]
    where buses = snd $ parse input
