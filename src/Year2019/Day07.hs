module Year2019.Day07
    ( part1
    , part2
    ) where

import Data.List (permutations)

import Year2019.IntCode


chain :: Memory -> [Int] -> [Int]
chain prog phases = ans
    where ans = foldr id (0 : ans) $ map (\phase input -> runWithInput (phase : input) prog) phases

part1 :: String -> Int
part1 (parse -> prog) = maximum $ map (head . chain prog) $ permutations [0..4]

part2 :: String -> Int
part2 (parse -> prog) = maximum $ map (last . chain prog) $ permutations [5..9]
