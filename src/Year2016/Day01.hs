module Year2016.Day01
    ( part1
    , part2
    ) where

import Data.HashSet (empty, insert, member)
import Data.List.Split
import Linear.V2


type Coord = V2 Int

turn :: Char -> Coord -> Coord
turn 'R' (V2 x y) = V2 y (-x)
turn 'L' (V2 x y) = V2 (-y) x
turn  _ _ = error "Invalid direction"

path :: String -> [Coord]
path = go (V2 0 1) (V2 0 0) . splitOn ", "
    where go _   _   []         = []
          go dir pos ((d:n):xs) = pathPart ++ go dir' (last pathPart) xs
              where dir' = turn d dir
                    pathPart = take (read n) . tail $ iterate (+ dir') pos
          go _ _ _ = error "Bad state"

part1 :: String -> Int
part1 = sum . abs . last . path

findDup :: [Coord] -> Coord
findDup = go empty
    where go _ [] = error "Bad state"
          go s (x:xs)
              | member x s = x
              | otherwise  = go (insert x s) xs

part2 :: String -> Int
part2 =  sum . abs . findDup . path
