{-# LANGUAGE DeriveGeneric #-}

module Year2018.Day22
    ( part1
    , part2
    ) where

import Data.Function.Memoize
import Data.Graph.AStar
import Data.Hashable
import Data.HashSet (HashSet)
import qualified Data.HashSet as S
import Data.Ix
import Data.Maybe
import GHC.Generics
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer


-- Look at ImplicitParams for (depth, target)

type Coord = (Int, Int)

parseInput :: String -> (Int, Coord)
parseInput = fromJust . parseMaybe parser
    where parser :: Parsec () String (Int, Coord)
          parser = do
            d <- string "depth: " *> decimal <* newline
            [x, y] <- string "target: " *> (decimal `sepBy` char ',')
            pure (d, (x, y))

erosionLevel :: (Int, Coord) -> Coord -> Int
erosionLevel (depth, target) = memoFix go
    where go f xy = (geologicIndex xy + depth) `mod` 20183
              where geologicIndex :: Coord -> Int
                    geologicIndex (0, 0) = 0
                    geologicIndex xy' | xy' == target = 0
                    geologicIndex (x, 0) = x * 16807
                    geologicIndex (0, y) = y * 48271
                    geologicIndex (x, y) = f (x-1, y) * f (x, y-1)

part1 :: String -> Int
part1 input = sum $ map ((`mod` 3) . erosionLevel (depth, target)) $ range ((0, 0), target)
    where (depth, target) = parseInput input

data Tool = Neither | Torch | ClimbingGear deriving (Eq, Generic, Ord, Show)
instance Hashable Tool

type Node = (Coord, Tool)

neighbors :: (Int, Coord) -> Node -> HashSet Node
neighbors (depth, target) ((x, y), t) = S.fromList poss
    where poss = filter (\((x', y'), t') -> x' >= 0 && y' >= 0 && viableMove t' (x', y'))
                 $ map (,t) [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]
                 ++ map ((x, y),) (filter (/=t) [Neither, Torch, ClimbingGear])
          viableMove t' xy = case t' of
                               Neither -> type' `elem` [1, 2]
                               Torch -> type' `elem` [0, 2]
                               ClimbingGear -> type' `elem` [0, 1]
              where type' = erosionLevel (depth, target) xy `mod` 3

time :: Node -> Node -> Int
time (_, t1) (_, t2)
    | t1 == t2 = 1
    | otherwise = 7

heur :: Coord -> Node -> Int
heur target ((x, y), _) = abs (x - fst target) + abs (y - snd target)

part2 :: String -> Int
part2 input = sum $ zipWith time path $ tail path
    where (depth, target) = parseInput input
          path = ((0, 0), Torch) :
                 fromJust (aStar (neighbors (depth, target))
                                 time
                                 (heur target)
                                 (== (target, Torch))
                                 ((0, 0), Torch))
