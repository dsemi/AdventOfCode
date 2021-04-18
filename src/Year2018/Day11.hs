module Year2018.Day11
    ( part1
    , part2
    ) where

import Control.Monad
import Data.Array.ST
import Data.Array.Unboxed
import Data.Bool
import Data.List (maximumBy)
import Data.Ord
import Linear.V2


makeSat :: Int -> UArray (V2 Int) Int
makeSat n = runSTUArray $ do
              sat <- newArray_ (V2 1 1, V2 300 300)
              forM_ [300,299..1] $ \x -> do
                forM_ [300,299..1] $ \y -> do
                  a <- bool (pure 0) (readArray sat (V2 x (y+1))) (y < 300)
                  b <- bool (pure 0) (readArray sat (V2 (x+1) y)) (x < 300)
                  c <- bool (pure 0) (readArray sat (V2 (x+1) (y+1))) (y < 300 && x < 300)
                  writeArray sat (V2 x y) (powerLevel (V2 x y) + a + b - c)
              pure sat
    where powerLevel (V2 x y) = let rackId = x + 10
                                in (rackId * y + n) * rackId `div` 100 `mod` 10 - 5

maxPartialSums :: UArray (V2 Int) Int -> [(V2 Int, Int)]
maxPartialSums sat = [ maximumBy (comparing snd)
                       [ (V2 x y, c) | V2 x y <- range (V2 1 1, V2 (300-size) (300-size))
                       , let c = sat ! V2 x y -
                                 sat ! V2 x (y+size) -
                                 sat ! V2 (x+size) y +
                                 sat ! V2 (x+size) (y+size) ]
                     | size <- [1..299] ]

part1 :: Int -> V2 Int
part1 = fst . (!! 2) . maxPartialSums . makeSat

part2 :: Int -> (Int, Int, Int)
part2 = (\(n, (V2 x y, _)) -> (x, y, n)) . maximumBy (comparing $ snd . snd)
        . zip [1..299] . maxPartialSums . makeSat
