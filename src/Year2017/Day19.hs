{-# LANGUAGE NegativeLiterals #-}

module Year2017.Day19
    ( part1
    , part2
    ) where

import Data.Array
import Linear.V2

data Dir = D | R | U | L deriving (Enum)

parse :: String -> Array (V2 Int) Char
parse input = accumArray (flip const) ' ' (V2 0 0, V2 rows cols) grid
    where rows = length (lines input) - 1
          cols = length (head (lines input)) - 1
          grid = [ (V2 r c, v) | (r, line) <- zip [0..] $ lines input
                 , (c, v) <- zip [0..] line ]

move :: Dir -> V2 Int -> V2 Int
move D = (+ V2  1  0)
move R = (+ V2  0  1)
move U = (+ V2 -1  0)
move L = (+ V2  0 -1)

turn :: Array (V2 Int) Char -> Dir -> V2 Int -> Dir
turn grid dir coord =
    case dir of
      D -> f R L
      R -> f U D
      U -> f L R
      L -> f D U
    where f a b
              | inRange (bounds grid) (move a coord) && grid ! move a coord /= ' ' = a
              | otherwise = b

followPath :: Array (V2 Int) Char -> String
followPath grid = go D firstCoord
    where firstCoord = fst . head . filter (\(V2 r _, v) -> r == 0 && v == '|') $ assocs grid
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
