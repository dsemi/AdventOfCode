{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Year2017.Day17
    ( part1
    , part2
    ) where

import Control.Lens
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
spinlock' n step = view _3 $ foldl' go (0, 0, 0) [1..n]
    where go (i, indexOf0, valueAfter0) v
              | i' <= indexOf0 = (i', indexOf0 + 1, valueAfter0)
              | i' == indexOf0 + 1 = (i', indexOf0, v)
              | otherwise = (i', indexOf0, valueAfter0)
              where i' = (i + step) `mod` v + 1
{-# INLINABLE spinlock' #-}

part2 :: Int -> Int
part2 = spinlock' (5*10^7)
