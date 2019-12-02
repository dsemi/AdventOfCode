module Year2019.Day01
    ( part1
    , part2
    ) where

import DaysTH


$(buildProb)

fuel :: Int -> Int
fuel mass = mass `div` 3 - 2

sumModuleFuels :: (Int -> Int) -> String -> Int
sumModuleFuels calcFuel = sum . map (calcFuel . read) . lines

part1' :: String -> Int
part1' = sumModuleFuels fuel

part2' :: String -> Int
part2' = sumModuleFuels $ sum . takeWhile (>0) . tail . iterate fuel
