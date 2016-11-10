module Year2015.Day18
    ( part1
    , part2
    ) where

import Data.Array.Unboxed
import Data.Ix

count x = length . filter (==x)

adjacents :: ((Int, Int), (Int, Int)) -> (Int, Int) -> [(Int, Int)]
adjacents bds (i, j) = [ (x, y)
                       | x <- [i-1 .. i+1]
                       , y <- [j-1 .. j+1]
                       , (x, y) /= (i, j)
                       , inRange bds (x, y)
                       ]

nextGrid f a = array bds [ (xy, f a xy adjStates)
                         | xy <- range bds
                         , let adjStates = map (a !) $ adjacents bds xy
                         ]
    where bds = bounds a

nextState :: UArray (Int, Int) Char -> (Int, Int) -> String -> Char
nextState a xy adjStates
    | a ! xy == '#' && not (lightsOn `elem` [2, 3]) = '.'
    | a ! xy == '.' && lightsOn == 3                = '#'
    | otherwise                                     = a ! xy
    where lightsOn = count '#' adjStates

makeGrid :: ([((Int, Int), Char)] -> [((Int, Int), Char)]) -> String -> UArray (Int, Int) Char
makeGrid f = accumArray seq '.' bds . f . zip (range bds) . concat . lines
    where bds = ((0, 0), (99, 99))

part1 :: String -> String
part1 = show . count '#' . elems . (!! 100)
        . iterate (nextGrid nextState) . makeGrid id

part2 :: String -> String
part2 = show . count '#' . elems . (!! 100)
        . iterate (nextGrid f) . makeGrid (++ zip corners "####")
    where corners = [(0, 0), (0, 99), (99, 0), (99, 99)]
          f a xy adjStates
              | xy `elem` corners = '#'
              | otherwise         = nextState a xy adjStates
