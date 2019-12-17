module Year2019.Day02
    ( part1
    , part2
    ) where

import Year2019.IntCode


part1 :: String -> Int
part1 = runNoIO 12 2 . parse

part2 :: String -> Int
part2 (parse -> vec) = head [ 100 * noun + verb
                            | noun <- [0..99], verb <- [0..99]
                            , runNoIO noun verb vec == 19690720
                            ]
