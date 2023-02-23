module Year2019.Day16
    ( part1
    , part2
    ) where

import Control.Monad.ST
import Data.Char
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV

part1 :: String -> String
part1 input = map intToDigit $ V.toList $ V.slice 0 8 $ go 100 nums
    where nums = V.fromList $ map digitToInt input
          len = V.length nums
          go c ns
              | c == 0 = ns
              | otherwise = go (c-1) $ V.generate len $ \n ->
                            let pos = sum [ V.sum (V.slice i (end-i) ns) | i <- [n, n + (n+1)*4 .. len-1]
                                          , let end = min len (i + n + 1)]
                                neg = sum [ V.sum (V.slice i (end-i) ns)
                                          | i <- [n+(n+1)*2, n+(n+1)*2 + (n+1)*4 .. len-1]
                                          , let end = min len (i + n + 1) ]
                            in abs (pos - neg) `mod` 10

pascalPeriod :: Int
pascalPeriod = 16000

pascal :: V.Vector Int
pascal = runST $ do
  vec <- MV.replicate pascalPeriod 0
  let go i v
          | i >= pascalPeriod = pure ()
          | otherwise = do
                   MV.write vec i v
                   go (i+1) ((v + 1) `mod` 10)
      inner idx
          | idx >= pascalPeriod = pure ()
          | otherwise = do
                   a <- MV.read vec (idx-1)
                   b <- MV.read vec idx
                   MV.write vec idx ((a + b) `mod` 10)
                   inner (idx+1)
      outer p
          | p >= 100 = pure ()
          | otherwise = do
                   MV.write vec 0 1
                   inner 1
                   outer (p+1)
  go 0 1
  outer 2
  V.freeze vec

part2 :: String -> String
part2 input = [ intToDigit $ (sumFirst * numCycles + sumLast) `mod` 10 | i <- [0..7]
              , let sumFirst = sum $ zipWith (\idx dig -> pascal V.! (idx `mod` pascalPeriod) * dig) [0..]
                               $ take jointCycle $ drop i $ cycle ds
              , let sumLast = sum $ zipWith (\idx dig -> pascal V.! (idx `mod` pascalPeriod) * dig) [0..]
                              $ drop (i + numCycles * jointCycle) $ take totLen $ cycle ds
              ]
    where offset = read $ take 7 input
          ns = map digitToInt input
          len = length ns
          ds = take len $ drop (offset `mod` len) $ cycle ns
          jointCycle = lcm pascalPeriod len
          totLen = len * 10000 - offset
          numCycles = totLen `div` jointCycle
