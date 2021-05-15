module Year2017.Day19
    ( part1
    , part2
    ) where

import Control.Lens
import Data.Array


type Coord = (Int, Int)
data Dir = D | R | U | L deriving (Enum)

parse :: String -> Array Coord Char
parse input = accumArray (flip const) ' ' ((0, 0), (rows, cols)) grid
    where rows = length (lines input) - 1
          cols = length (head (lines input)) - 1
          grid = [ ((r, c), v) | (r, line) <- zip [0..] $ lines input
                 , (c, v) <- zip [0..] line ]

move :: Dir -> Coord -> Coord
move D = over _1 succ
move R = over _2 succ
move U = over _1 pred
move L = over _2 pred

turn :: Array Coord Char -> Dir -> Coord -> Dir
turn grid dir coord =
    case dir of
      D -> f R L
      R -> f U D
      U -> f L R
      L -> f D U
    where f a b
              | inRange (bounds grid) (move a coord) && grid ! move a coord /= ' ' = a
              | otherwise = b

followPath :: Array Coord Char -> String
followPath grid = go D firstCoord
    where firstCoord = fst . head . filter (\((r, _), v) -> r == 0 && v == '|') $ assocs grid
          go dir coord
              | not (inRange (bounds grid) nextCoord) || grid ! nextCoord == ' ' =
                  if grid ! coord /= '+'
                  then [grid ! coord]
                  else go nextDir coord
              | otherwise = grid ! coord : go dir nextCoord
              where nextCoord = move dir coord
                    nextDir = turn grid dir coord

part1 :: String -> String
part1 = filter (not . (`elem` "|-+")) . followPath . parse

part2 :: String -> Int
part2 = length . followPath . parse
