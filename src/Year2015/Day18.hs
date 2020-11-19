module Year2015.Day18
    ( part1
    , part2
    ) where

import Data.Array.Unboxed


count :: Eq a => a -> [a] -> Int
count x = length . filter (==x)

adjacents :: UArray (Int, Int) Char -> (Int, Int) -> [Char]
adjacents a (i, j) = [ a ! (x, y) | x <- [i-1 .. i+1]
                     , y <- [j-1 .. j+1]
                     , (x, y) /= (i, j)
                     , inRange (bounds a) (x, y)
                     ]

nextGrid :: UArray (Int, Int) Char -> UArray (Int, Int) Char
nextGrid a = array bds [ (xy, nextState xy adjStates)
                         | xy <- range bds
                         , let adjStates = adjacents a xy
                         ]
    where bds = bounds a
          nextState xy adjStates
              | a ! xy == '#' && not (lightsOn `elem` [2, 3]) = '.'
              | a ! xy == '.' && lightsOn == 3                = '#'
              | otherwise                                     = a ! xy
              where lightsOn = count '#' adjStates

makeGrid :: String -> UArray (Int, Int) Char
makeGrid = array bds . zip (range bds) . concat . lines
    where bds = ((0, 0), (99, 99))

part1 :: String -> Int
part1 = count '#' . elems . (!! 100) . iterate nextGrid . makeGrid

part2 :: String -> Int
part2 = count '#' . elems . (!! 100) . iterate (fixCorners . nextGrid) . fixCorners . makeGrid
    where fixCorners = (// zip [(0, 0), (0, 99), (99, 0), (99, 99)] (repeat '#'))
