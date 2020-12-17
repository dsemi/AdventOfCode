module Year2020.Day17
    ( part1
    , part2
    ) where

import Control.Lens
import Data.Array.Unboxed
import Linear.V3
import Linear.V4


type Grid f = UArray (f Int) Bool

parse :: String -> Grid V3
parse input = array bds grid
    where grid = [ (V3 x y 0, v == '#')
                 | (y, row) <- zip [0..] $ lines input
                 , (x, v) <- zip [0..] row]
          bds = (minimum (map fst grid), maximum (map fst grid))

neighbors :: (Ix (f Int), Traversable f) => Grid f -> f Int -> Int
neighbors grid coord = sum [ 1 | coord' <- traverse (\x -> [x-1, x, x+1]) coord
                           , coord' /= coord
                           , inRange (bounds grid) coord'
                           , grid ! coord' ]

step :: (Ix (f Int), Traversable f) => Grid f -> Grid f
step grid = listArray bounds' $ map f $ range bounds'
    where (a, b) = bounds grid
          bounds' = (subtract 1 <$> a, (+1) <$> b)
          getVal coord = inRange (a, b) coord && grid ! coord
          f coord = count == 3 || getVal coord && count == 2
              where count = neighbors grid coord

countAfterSix :: (Ix (f Int), Traversable f) => Grid f -> Int
countAfterSix = length . filter id . elems . (!! 6) . iterate step

part1 :: String -> Int
part1 = countAfterSix . parse

part2 :: String -> Int
part2 (parse -> grid) = countAfterSix grid'
    where grid' = ixmap (both %~ (\(V3 x y z) -> V4 x y z 0) $ bounds grid) (^._xyz) grid
