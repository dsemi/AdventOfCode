module Year2016.Day01
    ( part1
    , part2
    ) where

import Control.Lens (_1, _2, over)
import Data.HashSet (empty, insert, member)
import Data.List.Split


type Coord = (Int, Int)
data Direction = North | East | South | West deriving (Enum)

turn :: Char -> Direction -> Direction
turn 'R' West  = North
turn 'R' dir   = succ dir
turn 'L' North = West
turn 'L' dir   = pred dir
turn  _ _ = error "Invalid direction"

move :: Direction -> Coord -> Coord
move North = over _2 succ
move East  = over _1 succ
move South = over _2 pred
move West  = over _1 pred

path :: String -> [Coord]
path = go North (0, 0) . splitOn ", "
    where go _   _   []         = []
          go dir pos ((d:n):xs) = pathPart ++ go dir' (last pathPart) xs
              where dir' = turn d dir
                    pathPart = take (read n) . tail $ iterate (move dir') pos
          go _ _ _ = error "Bad state"

manhattanDist :: Coord -> Int
manhattanDist (a, b) = abs a + abs b

part1 :: String -> Int
part1 = manhattanDist . last . path

findDup :: [Coord] -> Coord
findDup = go empty
    where go _ [] = error "Bad state"
          go s (x:xs)
              | member x s = x
              | otherwise  = go (insert x s) xs

part2 :: String -> Int
part2 =  manhattanDist . findDup . path
