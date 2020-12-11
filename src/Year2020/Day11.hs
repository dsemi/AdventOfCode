module Year2020.Day11
    ( part1
    , part2
    ) where

import Data.Array.Unboxed
import Linear.V2


type Coord = V2 Int
type Grid = UArray Coord Char

parse :: String -> Grid
parse inp = array (fst (head grid), fst (last grid)) grid
    where grid = [ (V2 x y, c) | (y, row) <- zip [0..] $ lines inp
                 , (x, c) <- zip [0..] row ]

dirs :: [Coord]
dirs = [ V2 (-1) (-1), V2 0 (-1), V2 1 (-1)
       , V2 (-1) 0, V2 1 0, V2 (-1) 1, V2 0 1, V2 1 1 ]

next :: Grid -> Coord -> (Coord, Char)
next grid xy = (xy, v)
    where v | grid ! xy == 'L' && numOcc == 0 = '#'
            | grid ! xy == '#' && numOcc >= 4 = 'L'
            | otherwise = grid ! xy
          numOcc = length $ filter (=='#') $ map (grid !)
                   $ filter (inRange (bounds grid)) $ map (+xy) dirs

stabilize :: (Grid -> Coord -> (Coord, Char)) -> Grid -> Int
stabilize f grid = length $ filter (=='#') $ elems $ fst $ head
                   $ filter (uncurry (==)) $ zip grids $ tail grids
    where nextState :: Grid -> Grid
          nextState g = array (bounds g) $ map (f g) $ indices grid
          grids = iterate nextState grid

part1 :: String -> Int
part1 = stabilize next . parse

next2 :: Grid -> Coord -> (Coord, Char)
next2 grid xy = (xy, v)
    where v | grid ! xy == 'L' && numOcc == 0 = '#'
            | grid ! xy == '#' && numOcc >= 5 = 'L'
            | otherwise = grid ! xy
          numOcc = length [ 1 | dir <- dirs
                          , let xs = dropWhile ((=='.') . (grid !))
                                     $ takeWhile (inRange (bounds grid))
                                     $ tail $ iterate (+dir) xy
                          , not (null xs) && grid ! head xs == '#' ]

part2 :: String -> Int
part2 = stabilize next2 . parse
