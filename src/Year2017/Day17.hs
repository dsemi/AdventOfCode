module Year2017.Day17
    ( part1
    , part2
    ) where

import Data.List (foldl')
import qualified Data.Sequence as S


spinlock :: Int -> Int -> Int
spinlock n step = let (i, s) = foldl' f (0, S.singleton 0) [1..n]
                  in S.index s $ (i + 1) `mod` S.length s
    where f (i, s) v = (i', S.insertAt i' v s)
              where i' = (i + step) `mod` v + 1

part1 :: Int -> Int
part1 = spinlock 2017

spinlock' :: Int -> Int -> Int
spinlock' v step = go (0, 0, 0)
    where go (pos, n, valAft0)
              | n >= v = valAft0
              | otherwise = go ((pos + skip * (step+1) - 1) `mod` n' + 1
                               , n'
                               , if pos == 1 then n else valAft0)
              where skip = (n - pos) `div` step + 1
                    n' = n + skip

part2 :: Int -> Int
part2 = spinlock' (5*10^7)
