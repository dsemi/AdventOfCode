module Year2016.Day24
    ( part1
    , part2
    ) where

import Control.Lens
import Data.Array
import Data.Char (digitToInt)
import Data.Graph.AStar
import Data.Hashable (Hashable, hashWithSalt)
import qualified Data.HashSet as S
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import Data.List (sortBy, permutations)
import Data.Ord (comparing)

data Node = Wall | Space | Target Int deriving (Eq, Ord, Show)

unwrap :: Node -> Int
unwrap Wall       = -2
unwrap Space      = -1
unwrap (Target n) = n

instance Hashable Node where
    hashWithSalt s n = hashWithSalt s $ unwrap n

type Grid = Array (Int, Int) Node

parseGrid :: String -> Grid
parseGrid s = array bds . concatMap (\(y, row) -> zipWith (\x c -> ((x, y), parseNode c))
                                                  [0..] row) . zip [0..] $ lines s
    where ubX = length (head (lines s)) - 1
          ubY = length (lines s) - 1
          bds = ((0, 0), (ubX, ubY))
          parseNode c
              | c == '#'            = Wall
              | c == '.'            = Space
              | c `elem` ['0'..'9'] = Target (digitToInt c)
              | otherwise = error "Invalid node"

findDistances :: Grid -> [((Int, Int), Node)] -> HashMap (Node, Node) Int
findDistances grid ns = M.fromList $ findDist <$> ns <*> ns
    where manhattanDist (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)
          neighbors (x, y) = S.fromList [ c | c <- [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]
                                        , inRange (bounds grid) c
                                        , grid ! c /= Wall
                                        ]
          findDist (p1, n1) (p2, n2)
              | p1 == p2  = ((n1, n2), 0)
              | otherwise = ((n1, n2), len)
              where Just len = length <$>
                               aStar neighbors (\_ -> const 1) (manhattanDist p2) (==p2) p1

allPathsAndDistanceMap :: Grid -> ([[Node]], HashMap (Node, Node) Int)
allPathsAndDistanceMap grid = (allPaths, findDistances grid pts)
    where pts = sortBy (comparing snd) . filter ((>=0) . unwrap . snd) $ assocs grid
          (start:targets) = map snd pts
          allPaths = map (start :) $ permutations targets

part1 :: String -> Int
part1 s = minimum $ map (\xs -> sum . map (distMap M.!) . zip xs $ tail xs) allPaths
    where (allPaths, distMap) = allPathsAndDistanceMap $ parseGrid s

part2 :: String -> Int
part2 s = minimum $ map (\xs -> sum . map (distMap M.!) . zip xs $ tail xs) allPaths
    where (allPaths, distMap) = over _1 (map (++[Target 0])) . allPathsAndDistanceMap $ parseGrid s
