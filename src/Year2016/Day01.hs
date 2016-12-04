module Year2016.Day01
    ( part1
    , part2
    ) where

import Control.Lens (_1, _2, over)
import Data.String.Utils (split)
import Data.HashSet (empty, insert, size)


type Coord = (Int, Int)
data Direction = North | East | South | West deriving (Enum)

turn :: Char -> Direction -> Direction
turn 'R' = right
    where right West = North
          right dir  = succ dir
turn 'L' = left
    where left North = West
          left dir   = pred dir

move :: Direction -> Coord -> Coord
move North = over _2 succ
move East  = over _1 succ
move South = over _2 pred
move West  = over _1 pred

path :: String -> [Coord]
path = go North (0, 0) . split ", "
    where go _ _ [] = []
          go dir pos ((d:n):xs) = pathPart ++ go dir' (last pathPart) xs
              where dir' = turn d dir
                    pathPart = take (read n) . tail $ iterate (move dir') pos

manhattanDist :: Coord -> Int
manhattanDist (a, b) = abs a + abs b

part1 :: String -> String
part1 = show . manhattanDist . last . path

findDup :: [Coord] -> Coord
findDup = go empty
    where go s []     = undefined
          go s (x:xs)
              | size s == size s' = x
              | otherwise = go s' xs
              where s' = insert x s

part2 :: String -> String
part2 = show . manhattanDist . findDup . path
