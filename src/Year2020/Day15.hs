module Year2020.Day15
    ( part1
    , part2
    ) where

import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.List.Split


run :: Int -> String -> Int
run n input = runST $ do
  let inp = map read $ splitOn "," input
  arr <- newArray (0, n) (-1) :: ST s (STUArray s Int Int)
  forM_ (zip [1..] inp) $ \(i, v) -> writeArray arr v i
  (\f -> foldM f 0 [length inp + 1 .. n-1]) $ \v turn -> do
    val <- readArray arr v
    let newV = if val == -1 then 0 else turn - val
    writeArray arr v turn
    pure newV

part1 :: String -> Int
part1 = run 2020

part2 :: String -> Int
part2 = run 30000000
