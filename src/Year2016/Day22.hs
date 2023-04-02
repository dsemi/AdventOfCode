module Year2016.Day22
    ( part1
    , part2
    ) where

import Control.Lens (view)
import Data.Array
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Char
import Data.Graph.AStar
import Data.HashSet (HashSet)
import qualified Data.HashSet as S
import Data.List (tails)
import Data.List.Split (splitOn)
import FlatParse.Basic
import Linear.V2

type Coord = V2 Int
data Node = Node { coords :: Coord
                 , used :: Int
                 , avail :: Int
                 } deriving (Eq, Show)

parse :: ByteString -> Node
parse line = case runParser parseNode line of
               OK res _ -> res
               _ -> error "unreachable"
    where parseNode = do
            p <- some $ satisfy (not . isSpace)
            let [_,'x':x,'y':y] = splitOn "-" p
            _ <- space *> int <* $(char 'T')
            u <- space *> int <* $(char 'T')
            a <- space *> int <* $(char 'T')
            _ <- space *> int <* $(char '%')
            pure $ Node (V2 (read x) (read y)) u a
          int = anyAsciiDecimalInt
          space = skipMany $ satisfy isSpace

viablePairs :: [Node] -> [(Node, Node)]
viablePairs nodes = [ (a, b) | (a:ns) <- init $ tails nodes
                    , b <- ns
                    , used a > 0 && used a < avail b || used b > 0 && used b < avail a
                    ]

part1 :: ByteString -> Int
part1 = length . viablePairs . map parse . drop 2 . B.lines

type Grid = Array Coord Node

buildGrid :: [Node] -> Grid
buildGrid ns = array (lb, ub) $ map (\n -> (coords n, n)) ns
    where lb = minimum $ map coords ns
          ub = maximum $ map coords ns

type GridState = (Coord,  Coord)

heuristic :: GridState -> Int
heuristic (o, t) = manhattanDist o t + manhattanDist t (V2 0 0)
    where manhattanDist a b = sum $ abs (b - a)

neighbors :: Grid -> GridState -> HashSet GridState
neighbors g (o, t) = S.fromList [ (o', if o' == t then o else t)
                                | o' <- map (o+) [V2 0 1, V2 0 (-1), V2 1 0, V2 (-1) 0]
                                , inRange (bounds g) o'
                                , not (isBlocked o')
                                ]
    where isBlocked c = used (g ! c) > 100

part2 :: ByteString -> Maybe Int
part2 s = let g = buildGrid . map parse . drop 2 $ B.lines s
              open = fst . head . filter ((==0) . used . snd) $ assocs g
              target = V2 (view _x . snd $ bounds g) 0
          in length <$> aStar (neighbors g) (\_ -> const 1) heuristic ((==V2 0 0) . snd) (open, target)
