module Year2017.Day15
    ( part1
    , part2
    ) where

import Data.Word (Word16)


parse :: String -> (Int, Int)
parse input =
    let [a, b] = map (read . last . words) $ lines input
    in (a, b)

judge :: (Int -> Int) -> (Int -> Int) -> Int -> (Int, Int) -> Int
judge nA nB = go 0
    where go c 0 _ = c
          go c n (a, b) = go c' (n-1) (a', b')
              where a' = nA a
                    b' = nB b
                    c' = if (fromIntegral a' :: Word16) == fromIntegral b'
                         then c + 1
                         else c

nextA :: Integral a => a -> a
nextA a = a * 16807 `mod` 2147483647

nextB :: Integral a => a -> a
nextB b = b * 48271 `mod` 2147483647

part1 :: String -> Int
part1 = judge nextA nextB (40*10^6) . parse

part2 :: String -> Int
part2 = judge fA fB (5*10^6) . parse
    where fA = until ((==0) . (`mod` 4)) nextA . nextA
          fB = until ((==0) . (`mod` 8)) nextB . nextB
