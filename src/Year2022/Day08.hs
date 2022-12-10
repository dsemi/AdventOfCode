module Year2022.Day08
    ( part1
    , part2
    ) where

import Data.Array.Unboxed
import Data.Char
import Linear.V2

data Tree = Tree { visibleFromEdge :: Bool, scenicScore :: Int }

trees :: String -> [Tree]
trees input = [ Tree (or vfes) (product sss) | (V2 r c, v) <- assocs grid
              , let paths = [ [grid ! (V2 nr c) | nr <- [r-1, r-2 .. 0]]
                            , [grid ! (V2 nr c) | nr <- [r+1..mr]]
                            , [grid ! (V2 r nc) | nc <- [c-1, c-2 .. 0]]
                            , [grid ! (V2 r nc) | nc <- [c+1..mc]]]
              , let (vfes, sss) = unzip $ zipWith (\a b -> let (la, lb) = (length a, length b)
                                                           in (la == lb, min la (lb + 1))) paths
                                  $ map (takeWhile (< v)) paths ]
    where ls = lines input
          bds@(_, V2 mr mc) = (V2 0 0, V2 (length ls - 1) (length (head ls) - 1))
          grid :: UArray (V2 Int) Int
          grid = array bds [ (V2 r c, digitToInt v) | (r, line) <- zip [0..] ls
                           , (c, v) <- zip [0..] line ]

part1 :: String -> Int
part1 = length . filter visibleFromEdge . trees

part2 :: String -> Int
part2 = maximum . map scenicScore . trees
