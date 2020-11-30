module Year2016.Day24
    ( part1
    , part2
    ) where

import Utils

import Control.Parallel.Strategies
import Data.Array.Unboxed
import Data.Char (digitToInt)
import Data.Graph.AStar
import qualified Data.HashSet as S
import Data.Map (Map)
import qualified Data.Map as M
import Data.List (sortBy, permutations)
import Data.Ord (comparing)
import Linear.V2


data Node = Wall | Space | Target Int deriving (Eq, Ord)

type Grid = Array (V2 Int) Node

parseGrid :: String -> Grid
parseGrid s = array bds [ (V2 x y, parseNode c) | (y, row) <- zip [0..] $ lines s
                        , (x, c) <- zip [0..] row]
    where ubX = length (head (lines s)) - 1
          ubY = length (lines s) - 1
          bds = (V2 0 0, V2 ubX ubY)
          parseNode c
              | c == '#'            = Wall
              | c == '.'            = Space
              | c `elem` ['0'..'9'] = Target (digitToInt c)
              | otherwise = error "Invalid node"

findDistances :: Grid -> [(V2 Int, Node)] -> Map (Node, Node) Int
findDistances grid ns = M.fromList $ findDist <$> ns <*> ns
    where manhattanDist a b = sum $ abs a - b
          neighbors xy = S.fromList [ c | c <- map (xy+) [V2 1 0, V2 (-1) 0, V2 0 1, V2 0 (-1)]
                                    , inRange (bounds grid) c
                                    , grid ! c /= Wall
                                    ]
          findDist (p1, n1) (p2, n2)
              | p1 == p2  = ((n1, n2), 0)
              | otherwise = ((n1, n2), len)
              where Just len = length <$>
                               aStar neighbors (\_ -> const 1) (manhattanDist p2) (==p2) p1

allPathsAndDistanceMap :: Grid -> ([[Node]], Map (Node, Node) Int)
allPathsAndDistanceMap grid = (allPaths, findDistances grid pts)
    where pts = sortBy (comparing snd) . filter (not . (`elem` [Wall, Space]) . snd) $ assocs grid
          (start:targets) = map snd pts
          allPaths = map (start :) $ permutations targets

minPathLen :: Map (Node, Node) Int -> [[Node]] -> Int
minPathLen distMap = minimum . parMap rseq (\xs -> sum . map (distMap M.!) . zip xs $ tail xs)

part1 :: String -> IO Int
part1 s = parallel $ minPathLen distMap allPaths
    where (allPaths, distMap) = allPathsAndDistanceMap $ parseGrid s

part2 :: String -> IO Int
part2 s = parallel $ minPathLen distMap $ map (++[Target 0]) allPaths
    where (allPaths, distMap) = allPathsAndDistanceMap $ parseGrid s
