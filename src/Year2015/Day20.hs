module Year2015.Day20
    ( part1
    , part2
    ) where

import Control.Monad (forM_)
import Control.Monad.ST (runST)
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as M


primes :: [Int]
primes = [2, 3, 5, 7, 11, 13]

solve :: Int -> Int -> Int
solve goal primeIndex
    | primeIndex < 0 = goal
    | otherwise = go 1 1 (solve goal $ primeIndex - 1)
    where p = primes !! primeIndex
          go pPower pSum best
              | pSum >= goal = best
              | otherwise = go pPower' pSum' $ min best (pPower' * solve subgoal (primeIndex - 1))
              where pPower' = pPower * p
                    pSum' = pSum + pPower'
                    subgoal = (goal + pSum' - 1) `div` pSum'

part1 :: Int -> Int
part1 n = solve (n `div` 10) $ length primes - 1

part2 :: Int -> Maybe Int
part2 n = V.findIndex (>=n) vec
    where vec :: Vector Int
          vec = runST $ do
                  let lim = n `div` 11
                  v <- M.new (lim + 1)
                  M.set v 0
                  forM_ [1..lim] $ \i -> do
                    forM_ (take 50 [i, 2*i..lim]) $ \j -> do
                      M.modify v (+ (i*11)) j
                  V.unsafeFreeze v
