{-# LANGUAGE FlexibleContexts #-}

module Year2020.Day11
    ( part1
    , part2
    ) where

import Data.Array.Unboxed
import qualified Data.HashMap.Strict as M
import Linear.V2


type Coord = V2 Int
type Grid = UArray Coord Char

parse :: String -> Grid
parse inp = array (fst (head grid), fst (last grid)) grid
    where grid = [ (V2 x y, c) | (y, row) <- zip [0..] $ lines inp
                 , (x, c) <- zip [0..] row ]

dirs :: [Coord]
dirs = [ V2 (-1) (-1), V2 0 (-1), V2 1 (-1), V2 (-1) 0
       , V2 1 0, V2 (-1) 1, V2 0 1, V2 1 1 ]

adjacents :: Grid -> Coord -> [Coord]
adjacents grid xy = filter (inRange (bounds grid)) $ map (+xy) dirs

stabilize :: Int -> (Grid -> Coord -> [Coord]) -> Grid -> Int
stabilize nThreshold adjs grid = length $ filter (=='#') $ elems $ fst $ head
                                 $ filter (uncurry (==)) $ zip grids $ tail grids
    where nextState :: Grid -> Grid
          nextState g = array (bounds g) $ map (next g) $ indices grid
          adjMap = M.fromList [ (k, adjs grid k) | (k, v) <- assocs grid, v /= '.' ]
          next g xy = (xy, v)
              where v | g ! xy == 'L' && numOcc == 0 = '#'
                      | g ! xy == '#' && numOcc >= nThreshold = 'L'
                      | otherwise = g ! xy
                    numOcc = length $ filter (=='#') $ map (g !) $ adjMap M.! xy
          grids = iterate nextState grid

part1 :: String -> Int
part1 = stabilize 4 adjacents . parse

adjacents2 :: Grid -> Coord -> [Coord]
adjacents2 grid xy = [ head xs | dir <- dirs
                     , let xs = dropWhile ((=='.') . (grid !))
                                $ takeWhile (inRange (bounds grid))
                                $ tail $ iterate (+dir) xy
                     , not (null xs) ]

part2 :: String -> Int
part2 = stabilize 5 adjacents2 . parse
