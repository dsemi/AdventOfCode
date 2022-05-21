module Year2021.Day20
    ( part1
    , part2
    ) where

import Control.Lens
import Data.Array.Unboxed
import Data.Bool
import Data.List (foldl')
import Data.List.Split (splitOn)
import Linear.V2

pad :: Char -> UArray (V2 Int) Char -> UArray (V2 Int) Char
pad v img = accumArray (flip const) v (V2 0 0, V2 (r+2) (c+2)) $ map (_1 %~ (+V2 1 1)) $ assocs img
    where (V2 r c) = snd $ bounds img

enhance :: UArray Int Char -> UArray (V2 Int) Char -> UArray (V2 Int) Char
enhance iea grid =
    let grid2 = grid' // [(k, adj k) | k <- range (V2 1 1, V2 (mr-1) (mc-1))]
        ch = (iea !) $ foldl' (\a b -> 2*a+(bool 0 1 (b=='#'))) 0 $ replicate 9 $ grid2 ! V2 0 0
    in grid2 // [(k, ch) | k@(V2 r c) <- range (bounds grid2), r `elem` [0, mr] || c `elem` [0, mc]]
    where grid' = pad (grid ! V2 0 0) grid
          (V2 mr mc) = snd $ bounds grid'
          adj (V2 r c) = (iea !) $ foldl' (\a b -> 2*a+(bool 0 1 (b=='#'))) 0 $ map (grid' !)
                         $ range (V2 (r-1) (c-1), V2 (r+1) (c+1))

run :: Int -> String -> Int
run times input = let [ieaL, img] = splitOn "\n\n" input
                      iea = listArray (0, length ieaL - 1) ieaL
                      imL = [ (V2 r c, v) | (r, line) <- zip [0..] $ lines img
                            , (c, v) <- zip [0..] line]
                      im = pad '.' $ array (V2 0 0, fst (last imL)) imL
                  in length $ filter (=='#') $ elems $ iterate (enhance iea) im !! times

part1 :: String -> Int
part1 = run 2

part2 :: String -> Int
part2 = run 50
