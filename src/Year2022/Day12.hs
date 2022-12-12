module Year2022.Day12
    ( part1
    , part2
    ) where

import Data.Array.Unboxed
import Linear.V2

import Utils

solve :: [Char] -> String -> Int
solve sts input = fst $ head $ filter ((==end) . snd) $ bfsMany starts neighbors
    where (mr, mc) = (length (lines input) - 1, (length (head $ lines input) - 1))
          iGrid :: UArray (V2 Int) Char
          iGrid = array (V2 0 0, V2 mr mc)
                  [ (V2 r c, v) | (r, line) <- zip [0..] $ lines input
                  , (c, v) <- zip [0..] line ]
          starts = map fst $ filter ((`elem` sts) . snd) $ assocs iGrid
          start = fst $ head $ filter ((=='S') . snd) $ assocs iGrid
          end = fst $ head $ filter ((=='E') . snd) $ assocs iGrid
          grid = iGrid // [(start, 'a'), (end, 'z')]
          neighbors pos = [ pos2 | pos2@(V2 r2 c2) <- map (+pos) [V2 (-1) 0, V2 1 0, V2 0 (-1), V2 0 1]
                          , r2 >= 0 && r2 <= mr && c2 >= 0 && c2 <= mc && grid ! pos2 <= lim ]
              where lim = succ $ grid ! pos

part1 :: String -> Int
part1 = solve "S"

part2 :: String -> Int
part2 = solve "Sa"
