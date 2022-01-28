module Year2021.Day17
    ( part1
    , part2
    ) where

import Linear.V2
import Data.Ix

import Utils

part1 :: String -> Int
part1 input = let [_, _, y0, _] = findAllInts input
              in y0 * (y0 + 1) `div` 2

hitsTarget :: V2 Int -> V2 Int -> V2 Int -> Bool
hitsTarget lo@(V2 _ y0) hi@(V2 x1 _) =
    any (inRange (lo, hi)) . takeWhile (\(V2 x y) -> x <= x1 && y >= y0)
    . map fst . iterate (\(ps, vs@(V2 x y)) -> (ps + vs, V2 (max 0 (x-1)) (y-1))) . (V2 0 0,)

-- First triangular number > x0 is lower bound.
-- n^2 + n - 2x0 = 0
part2 :: String -> Int
part2 input = let [x0, x1, y0, y1] = findAllInts input
                  mx = ceiling $ sqrt (1.0 + 8.0 * fromIntegral x0) / 2.0 - 0.5
              in length [ () | x <- [mx .. x1]
                        , y <- [y0 .. -y0]
                        , hitsTarget (V2 x0 y0) (V2 x1 y1) (V2 x y) ]
