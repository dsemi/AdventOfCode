module Year2021.Day09
    ( part1
    , part2
    ) where

import Data.Char
import qualified Data.HashSet as S
import Data.List (sort)
import Data.Vector ((!), (!?), Vector)
import qualified Data.Vector as V
import Linear.V2

import Utils

neighbs :: Vector (Vector Int) -> V2 Int -> [V2 Int]
neighbs grid x = [ V2 r c | (V2 r c) <- map (x+) [V2 (-1) 0, V2 1 0, V2 0 (-1), V2 0 1]
                 , Just _ <- [grid !? r >>= (!? c)] ]

lows :: Vector (Vector Int) -> [V2 Int]
lows grid = [ pt | r <- [0 .. V.length grid - 1]
            , c <- [0 .. V.length (grid ! 0) - 1]
            , let pt = V2 r c
            , let v = grid ! r ! c
            , all (\(V2 x y) -> v < grid ! x ! y) $ neighbs grid pt ]

part1 :: String -> Int
part1 input = sum $ map (\(V2 r c) -> 1 + grid ! r ! c) $ lows grid
    where grid = V.fromList $ map (V.fromList . map digitToInt) $ lines input

part2 :: String -> Int
part2 input = product $ take 3 $ reverse $ sort basins
    where grid = V.fromList $ map (V.fromList . map digitToInt) $ lines input
          neighbs' = filter (\(V2 r c) -> grid ! r ! c /= 9) . neighbs grid
          (basins, _) = foldr (\pt (bs, s) -> let (b, s') = go 0 s (bfs pt neighbs')
                                              in (b:bs, s'))
                        ([], S.empty) $ lows grid
          go c s [] = (c, s)
          go c s (p:ps)
              | S.member p s = (c, s)
              | otherwise = go (c+1) (S.insert p s) ps
