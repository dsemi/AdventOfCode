module Year2016.Day13
    ( part1
    , part2
    ) where

import Utils

import Data.Bits (popCount)


type Coord = (Int, Int)

target :: Coord
target = (31, 39)

neighbors :: Int -> Coord -> [Coord]
neighbors n (x, y) = filter isOpen $ filter isValid [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]
    where isOpen (x', y') = even $ popCount $ x'*x' + 3*x' + 2*x'*y' + y' + y'*y' + n
          isValid (x', y') = x' >= 0 && y' >= 0

part1 :: Int -> Int
part1 = fst . head . filter ((==target) . snd) . bfs (1, 1) . neighbors

part2 :: Int -> Int
part2 = length . takeWhile ((<=50) . fst) . bfs (1, 1) . neighbors
