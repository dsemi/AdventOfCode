{-# LANGUAGE ViewPatterns #-}

module Year2019.Day07
    ( part1
    , part2
    ) where

import Data.List (permutations)

import Year2019.IntCode


chain :: Bool -> Memory -> [Int] -> Int
chain feedback prog phases = last ans
    where ans = foldr id firstInput $ map (\phase -> \input -> runV2 (phase : input) prog) phases
          firstInput = if feedback then (0 : ans) else [0]

part1 :: String -> Int
part1 (parse -> prog) = maximum $ map (chain False prog) $ permutations [0..4]

part2 :: String -> Int
part2 (parse -> prog) = maximum $ map (chain True prog) $ permutations [5..9]
