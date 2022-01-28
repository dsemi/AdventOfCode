module Year2021.Day15
    ( part1
    , part2
    ) where

import Data.Array.Unboxed
import Data.Char
import Data.Graph.AStar
import Data.HashSet (HashSet)
import qualified Data.HashSet as S
import Linear.V2

neighbors :: UArray (V2 Int) Int -> V2 Int -> HashSet (V2 Int)
neighbors grid = S.fromList . go
    where bds = bounds grid
          go pos = [ p | p <- map (pos+) [V2 1 0, V2 0 1, V2 (-1) 0, V2 0 (-1)]
                   , inRange bds p ]

parse :: String -> UArray (V2 Int) Int
parse input = array (V2 0 0, V2 (rows-1) (cols-1))
              [ (V2 r c, digitToInt v) | (r, row) <- zip [0..] $ lines input
              , (c, v) <- zip [0..] row ]
    where rows = length $ lines input
          cols = length $ head $ lines input

part1 :: String -> Maybe Int
part1 input = sum . map (grid !) <$> aStar (neighbors grid) (const (grid !))
              (sum . abs . subtract goal) (== goal) (V2 0 0)
    where grid = parse input
          goal = snd $ bounds grid

part2 :: String -> Maybe Int
part2 input = sum . map (grid !) <$> aStar (neighbors grid) (const (grid !))
              (sum . abs . subtract goal) (== goal) (V2 0 0)
    where expand g = array (V2 0 0, V2 (mr*5-1) (mc*5-1))
                     [ (V2 r c, v) | r <- [0..mr*5-1]
                     , c <- [0..mc*5-1]
                     , let (rd, rm) = r `divMod` mr
                           (cd, cm) = c `divMod` mc
                     , let v = (g ! V2 rm cm - 1 + rd + cd) `mod` 9 + 1]
              where V2 mr mc = snd (bounds g) + V2 1 1
          grid = expand $ parse input
          goal = snd $ bounds grid
