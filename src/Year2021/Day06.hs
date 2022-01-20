module Year2021.Day06
    ( part1
    , part2
    ) where

import Data.Char
import Control.Monad
import Control.Monad.ST
import qualified Data.Vector.Unboxed.Mutable as MV

solve :: Int -> String -> Int
solve n s = runST $ do
  v <- MV.generate 9 (\x -> length $ filter (==intToDigit x) s)
  forM_ [0..n-1] $ \i ->
      MV.read v (i `mod` 9) >>= \x -> MV.modify v (+x) ((i + 7) `mod` 9)
  MV.foldl' (+) 0 v

part1 :: String -> Int
part1 = solve 80

part2 :: String -> Int
part2 = solve 256
