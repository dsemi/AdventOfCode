{-# LANGUAGE ViewPatterns #-}

module Year2019.Day07
    ( part1
    , part2
    ) where

import Data.List (permutations)

import Year2019.IntCode


chain :: Bool -> Memory -> [Int] -> Int
chain feedback prog phases = last $ last outputs
    where outputs = zipWith f [0..] phases
              where f 0 phase = let inp = if feedback then last outputs else []
                                in runV2 (phase : 0 : inp) prog
                    f i phase = runV2 (phase : outputs !! (i - 1)) prog

part1 :: String -> Int
part1 (parse -> prog) = maximum $ map (chain False prog) $ permutations [0..4]

part2 :: String -> Int
part2 (parse -> prog) = maximum $ map (chain True prog) $ permutations [5..9]
