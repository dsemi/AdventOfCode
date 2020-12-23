{-# LANGUAGE FlexibleContexts #-}

module Year2020.Day23
    ( part1
    , part2
    ) where

import Data.Char
import Data.Array.ST
import Data.Array.Unboxed
import Data.Array.Unsafe


parse :: [Int] -> (Int, UArray Int Int)
parse nums = (head nums, array (minimum nums, maximum nums) $ zip nums $ tail nums ++ [head nums])

run :: Int -> Int -> UArray Int Int -> UArray Int Int
run steps start arr = runSTUArray $ do
                        m <- unsafeThaw arr
                        step steps m start
    where max' = maximum $ elems arr
          step cnt m x
              | cnt == 0 = pure m
              | otherwise = do
                  a <- readArray m x
                  b <- readArray m a
                  c <- readArray m b
                  readArray m c >>= writeArray m x
                  let n = head [ y' | y <- [x-1, x-2 ..]
                               , let y' = if y < 1 then y + max' else y
                               , y' `notElem` [a, b, c] ]
                  readArray m n >>= writeArray m c
                  writeArray m n a
                  readArray m x >>= step (cnt-1) m

part1 :: String -> String
part1 input = let (start, m) = parse $ map digitToInt input
                  m' = run 100 start m
              in map intToDigit $ takeWhile (/= 1) $ tail $ iterate (m' !) 1

part2 :: String -> Int
part2 input = let (start, m) = parse $ map digitToInt input ++ [10..10^6]
                  m' = run (10^7) start m
              in m' ! 1 * m' ! (m' ! 1)
