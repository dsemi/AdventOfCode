{-# LANGUAGE BangPatterns, FlexibleContexts #-}

module Year2020.Day23
    ( part1
    , part2
    ) where

import qualified Control.Foldl as L
import Data.Char
import Data.Array.Base
import Data.Array.ST


parse :: [Int] -> (Int, UArray Int Int)
parse nums = (head nums, array (0, mx) $ (end, head nums) : (zip nums $ tail nums))
    where (Just mx, Just end) = L.fold ((,) <$> L.maximum <*> L.last) nums

run :: Int -> Int -> UArray Int Int -> UArray Int Int
run steps start arr = runSTUArray $ unsafeThaw arr >>= flip (step steps) start
    where mx = snd $ bounds arr
          next x = if x == 1 then mx else x - 1
          step !cnt m x
              | cnt == 0 = pure m
              | otherwise = do
                  a <- unsafeRead m x
                  b <- unsafeRead m a
                  c <- unsafeRead m b
                  unsafeRead m c >>= unsafeWrite m x
                  let go !z
                          | z == a || z == b || z == c = go $ next z
                          | otherwise = z
                      n = go $ next x
                  unsafeRead m n >>= unsafeWrite m c
                  unsafeWrite m n a
                  unsafeRead m x >>= step (cnt-1) m

part1 :: String -> String
part1 input = let m = uncurry (run 100) $ parse $ map digitToInt input
              in map intToDigit $ takeWhile (/= 1) $ tail $ iterate (m !) 1

part2 :: String -> Int
part2 input = let m = uncurry (run (10^7)) $ parse $ map digitToInt input ++ [10..10^6]
              in m ! 1 * m ! (m ! 1)
