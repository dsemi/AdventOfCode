{-# LANGUAGE NumericUnderscores #-}

module Year2020.Day15
    ( part1
    , part2
    ) where

import Control.Monad
import Control.Monad.ST
import Data.List.Split
import qualified Data.Vector.Unboxed.Mutable as V


run :: Int -> String -> Int
run n input = runST $ do
  let inp = map read $ splitOn "," input
  arr <- V.replicate (n+1) (-1)
  forM_ (zip [1..] inp) $ \(i, v) -> V.unsafeWrite arr v i
  (\f -> foldM f 0 [length inp + 1 .. n-1]) $ \v i -> do
    val <- V.unsafeRead arr v <* V.unsafeWrite arr v i
    pure $ if val == -1 then 0 else i - val

part1 :: String -> Int
part1 = run 2020

part2 :: String -> Int
part2 = run 30_000_000
