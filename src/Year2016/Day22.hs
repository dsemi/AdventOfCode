module Year2016.Day22
    ( part1
    , part2
    ) where

import Data.Array
import Data.Graph.AStar
import Data.HashSet (HashSet)
import qualified Data.HashSet as S
import Data.Maybe (mapMaybe)
import Data.List (tails)
import Data.List.Split (splitOn)
import Text.Megaparsec
import Text.Megaparsec.Char (char, noneOf, space)
import Text.Megaparsec.Char.Lexer (decimal)


type Coord = (Int, Int)
data Node = Node { coords :: Coord
                 , used :: Int
                 , avail :: Int
                 } deriving (Eq, Show)

parseNode :: Parsec () String Node
parseNode = do
  p <- some $ noneOf " "
  let [_,'x':x,'y':y] = splitOn "-" p
  _ <- space *> int <* char 'T'
  u <- space *> int <* char 'T'
  a <- space *> int <* char 'T'
  _ <- space *> int <* char '%'
  return $ Node (read x, read y) u a
    where int = fromInteger <$> decimal

viablePairs :: [Node] -> [(Node, Node)]
viablePairs nodes = [ (a, b) | (a:ns) <- init $ tails nodes
                    , b <- ns
                    , used a > 0 && used a < avail b || used b > 0 && used b < avail a
                    ]

part1 :: String -> Int
part1 = length . viablePairs . mapMaybe (parseMaybe parseNode) . drop 2 . lines

type Grid = Array Coord Node

buildGrid :: [Node] -> Grid
buildGrid ns = array (lb, ub) $ map (\n -> (coords n, n)) ns
    where lb = minimum $ map coords ns
          ub = maximum $ map coords ns

type GridState = (Coord,  Coord)

heuristic :: GridState -> Int
heuristic (o, t) = manhattanDist o t + manhattanDist t (0, 0)
    where manhattanDist (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

neighbors :: Grid -> GridState -> HashSet GridState
neighbors g (o@(ox, oy), t) = S.fromList [ (o', if o' == t then o else t)
                                         | o' <- [(ox, oy+1), (ox, oy-1), (ox+1, oy), (ox-1, oy)]
                                         , inRange (bounds g) o'
                                         , not (isBlocked o')
                                         ]
    where isBlocked c = used (g ! c) > 100

part2 :: String -> Int
part2 s = let g =  buildGrid . mapMaybe (parseMaybe parseNode) . drop 2 $ lines s
              open = fst . head . filter ((==0) . used . snd) $ assocs g
              target = (fst . snd $ bounds g, 0)
              Just len = length <$> aStar (neighbors g) (\_ -> const 1) heuristic ((==(0,0)) . snd) (open, target)
          in len
