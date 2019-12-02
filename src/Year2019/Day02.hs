{-# LANGUAGE ViewPatterns #-}

module Year2019.Day02
    ( part1
    , part2
    ) where

import DaysTH
import Year2019.IntCode (parse, run)


$(buildProb)

part1' :: String -> Int
part1' = run 12 2 . parse

part2' :: String -> Int
part2' (parse -> vec) = head [ 100 * noun + verb
                             | noun <- [0..99], verb <- [0..99]
                             , run noun verb vec == 19690720
                             ]
