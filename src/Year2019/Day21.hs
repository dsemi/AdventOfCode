module Year2019.Day21
    ( part1
    , part2
    ) where

import Data.Char

import Year2019.IntCode

run :: [String] -> String -> Int
run instrs = last . runWithInput (map ord $ unlines instrs) . parse

part1 :: String -> Int
part1 = run [ "OR A T"
            , "AND B T"
            , "AND C T"
            , "NOT T J"
            , "AND D J"
            , "WALK"
            ]

part2 :: String -> Int
part2 = run [ "OR A T"
            , "AND B T"
            , "AND C T"
            , "NOT T J"
            , "AND D J"
            , "NOT J T"
            , "OR E T"
            , "OR H T"
            , "AND T J"
            , "RUN"
            ]
