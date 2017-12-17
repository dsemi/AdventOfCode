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
spinlock' n step = go (0, 0, 0) 1
    where go (i, indexOf0, valueAfter0) v
              | v > n = valueAfter0
              | i' <= indexOf0 = go (i', indexOf0 + 1, valueAfter0) $ v + 1
              | i' == indexOf0 + 1 = go (i', indexOf0, v) $ v + 1
              | otherwise = go (i', indexOf0, valueAfter0) $ v + 1
              where i' = (i + step) `mod` v + 1

part2 :: Int -> Int
part2 = spinlock' (5*10^7)
