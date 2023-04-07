{-# LANGUAGE DeriveGeneric, NegativeLiterals, TypeFamilies #-}

module Year2018.Day22
    ( part1
    , part2
    ) where

import Control.Lens
import Data.Array
import Data.ByteString (ByteString)
import Data.Graph.AStar
import Data.Hashable
import Data.HashSet (HashSet)
import qualified Data.HashSet as S
import Data.Maybe
import FlatParse.Basic
import GHC.Generics
import Linear.V2

type Coord = V2 Int

data Tool = Neither | Torch | ClimbingGear deriving (Enum, Eq, Generic, Ord, Show)
instance Hashable Tool

next :: Tool -> Tool
next ClimbingGear = Neither
next x = succ x

parseInput :: ByteString -> (Int, Coord)
parseInput input = case runParser parser input of
                     OK res _ -> res
                     _ -> error "unreachable"
    where parser = do
            d <- $(string "depth: ") *> anyAsciiDecimalInt <* $(char '\n')
            pt <- V2 <$> ($(string "target: ") *> anyAsciiDecimalInt) <*>
                  ($(char ',') *> anyAsciiDecimalInt)
            pure (d, pt)

erosionLevels :: Int -> Coord -> Array Coord Tool
erosionLevels depth target = fmap (toEnum . (`mod` 3)) arr
    where mx = maximum target + 3 -- Arbitrary buffer size for search
          arr = listArray (V2 0 0, V2 mx mx) $ map go $ range (V2 0 0, V2 mx mx)
          go xy = (geologicIndex xy + depth) `mod` 20183
              where geologicIndex :: Coord -> Int
                    geologicIndex (V2 0 0) = 0
                    geologicIndex xy' | xy' == target = 0
                    geologicIndex (V2 x 0) = x * 16807
                    geologicIndex (V2 0 y) = y * 48271
                    geologicIndex (V2 x y) = arr ! V2 (x - 1) y * arr ! V2 x (y - 1)

part1 :: ByteString -> Int
part1 input = sum $ map (fromEnum . snd) $ filter (inRange (V2 0 0, target) . fst)
              $ assocs $ erosionLevels depth target
    where (depth, target) = parseInput input

type Node = (Coord, Tool)

neighbors :: Int -> Coord -> Node -> HashSet Node
neighbors depth target node = S.fromList poss
    where els = erosionLevels depth target
          poss = filter (\node' -> inRange (bounds els) (fst node') && viableMove node')
                 $ traverseOf _1 (\x -> map (+x) [V2 -1 0, V2 1 0, V2 0 -1, V2 0 1]) node
                 ++ map (\f -> over _2 f node) [next, next . next]
          viableMove (xy, t) = t /= els ! xy

time :: Node -> Node -> Int
time (_, t1) (_, t2)
    | t1 == t2 = 1
    | otherwise = 7

heur :: Coord -> Node -> Int
heur target (xy, _) = sum $ abs $ target - xy

part2 :: ByteString -> Int
part2 input = sum $ zipWith time path $ tail path
    where (depth, target) = parseInput input
          path = (V2 0 0, Torch) :
                 fromJust (aStar (neighbors depth target)
                                 time
                                 (heur target)
                                 (== (target, Torch))
                                 (V2 0 0, Torch))
