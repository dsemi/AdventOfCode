module Year2015.Day20
    ( part1
    , part2
    ) where

import Control.Monad (forM_)
import Control.Monad.ST (runST)
import Data.Maybe (fromJust)
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as M


part1 :: Int -> Int
part1 n = fromJust $ V.findIndex (>=n) vec
    where vec :: Vector Int
          vec = runST $ do
                  let lim = n `div` 10
                  v <- M.new (lim + 1)
                  M.set v 0
                  forM_ [1..lim] $ \i -> do
                    forM_ [i, 2*i..lim] $ \j -> do
                      M.modify v (+ (i*10)) j
                  V.unsafeFreeze v

part2 :: Int -> Int
part2 n = fromJust $ V.findIndex (>=n) vec
    where vec :: Vector Int
          vec = runST $ do
                  let lim = n `div` 11
                  v <- M.new (lim + 1)
                  M.set v 0
                  forM_ [1..lim] $ \i -> do
                    forM_ (take 50 [i, 2*i..lim]) $ \j -> do
                      M.modify v (+ (i*11)) j
                  V.unsafeFreeze v
