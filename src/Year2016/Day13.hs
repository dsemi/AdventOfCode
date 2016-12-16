{-# LANGUAGE TupleSections #-}

module Year2016.Day13
    ( part1
    , part2
    ) where

import Utils (aStar)

import Data.Bits (popCount)
import Data.Hashable (Hashable)
import Data.HashSet (HashSet)
import qualified Data.HashSet as S
import Data.Maybe (fromJust)

isOpen :: Int -> (Int, Int) -> Bool
isOpen n (x, y) = even $ popCount $ x*x + 3*x + 2*x*y + y + y*y + n

target :: (Int, Int)
target = (31, 39)

heuristic :: (Int, Int) -> Int
heuristic (x, y) = abs (x - fst target) + abs (y - snd target)

neighbors :: ((Int, Int) -> Bool) -> (Int, Int) -> [(Int, Int)]
neighbors isOpen (x, y) = filter isOpen $ filter isValid [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]
    where isValid (x, y) = x >=0 && y >= 0

part1 :: String -> Int
part1 s = fromJust $ aStar (1, 1) (==target) heuristic (neighbors (isOpen n))
    where n = read s

bfs :: (Eq a, Hashable a) => a -> Int -> (a -> [a]) -> HashSet a
bfs start depth neighbors = search [(0, start)] S.empty
    where search [] visited = visited
          search ((d, node):ns) visited
              | d == depth || S.member node visited = search ns visited'
              | otherwise = search (ns ++ map (d+1,) (neighbors node)) visited'
              where visited' = S.insert node visited

part2 :: String -> Int
part2 s = S.size $ bfs (1, 1) 50 (neighbors (isOpen n))
    where n = read s
