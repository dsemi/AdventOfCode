{-# LANGUAGE DeriveGeneric, TemplateHaskell, TypeApplications #-}

module Year2018.Day22
    ( part1
    , part2
    ) where

import DaysTH

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


type Coord = (Int, Int)

depth :: Int
target :: Coord
(depth, target) = fromJust $ parseMaybe @() (do
                    d <- string "depth: " *> decimal <* newline
                    [x, y] <- string "target: " *> (decimal `sepBy` char ',')
                    pure (d, (x, y))) ($(input) :: String)

geologicIndex :: Coord -> Int
geologicIndex = memoize go
    where go (0, 0) = 0
          go xy | xy == target = 0
          go (x, 0) = x * 16807
          go (0, y) = y * 48271
          go (x, y) = erosionLevel (x-1, y) * erosionLevel (x, y-1)

erosionLevel :: Coord -> Int
erosionLevel = memoize go
    where go xy = (geologicIndex xy + depth) `mod` 20183

part1 :: String -> Int
part1 _ = sum $ map ((`mod` 3) . erosionLevel) $ range ((0, 0), target)

data Tool = Neither | Torch | ClimbingGear deriving (Eq, Generic, Ord, Show)
instance Hashable Tool

type Node = (Coord, Tool)

neighbors :: Node -> HashSet Node
neighbors ((x, y), t) = S.fromList poss
    where poss = filter (\((x', y'), t') -> x' >= 0 && y' >= 0 && viableMove t' (x', y'))
                 $ map (,t) [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]
                 ++ map ((x, y),) (filter (/=t) [Neither, Torch, ClimbingGear])
          viableMove t' xy = case t' of
                               Neither -> type' `elem` [1, 2]
                               Torch -> type' `elem` [0, 2]
                               ClimbingGear -> type' `elem` [0, 1]
              where type' = erosionLevel xy `mod` 3

time :: Node -> Node -> Int
time (_, t1) (_, t2)
    | t1 == t2 = 1
    | otherwise = 7

heur :: Node -> Int
heur ((x, y), _) = abs (x - fst target) + abs (y - snd target)

part2 :: String -> Int
part2 _ = let path = ((0, 0), Torch) :
                     fromJust (aStar neighbors time heur (== (target, Torch)) ((0, 0), Torch))
          in sum $ zipWith time path $ tail path
