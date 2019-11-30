module Year2018.Day11
    ( part1
    , part2
    ) where

import Control.Arrow
import Data.Array.Unboxed
import Data.List (maximumBy)
import Data.Ord


type Coord = (Int, Int)

powerLevels :: Int -> UArray Coord Int
powerLevels n = array ((1, 1), (300, 300)) $ map (id &&& f) $ range ((1, 1), (300, 300))
    where f (x, y) = let rackId = x + 10
                     in (rackId * y + n) * rackId `div` 100 `mod` 10 - 5

-- Power level of:
-- [x][x][x][x]
-- [x][x][x][x]
-- [x][x][x][x]
-- [x][x][x][x]
-- is
-- [x][x][x] x     x  x  x  x     x  x  x [x]    x  x  x  x     x  x  x  x
-- [x][x][x] x  +  x [x][x][x] +  x  x  x  x  +  x  x  x  x  -  x [x][x] x
-- [x][x][x] x     x [x][x][x]    x  x  x  x     x  x  x  x     x [x][x] x
--  x  x  x  x     x [x][x][x]    x  x  x  x    [x] x  x  x     x  x  x  x
maxPartialSums :: UArray Coord Int -> [(Coord, Int)]
maxPartialSums grid = map (maximumBy (comparing snd) . assocs) $ grid : go 1 (const 0) (grid !)
    where go :: Int -> (Coord -> Int) -> (Coord -> Int) -> [UArray Coord Int]
          go n xyLookup'' xyLookup' = nextGrid : go (n+1) xyLookup' (nextGrid !)
              where nextGrid = array bds $ map (id &&& f) $ range bds
                    bds = ((1, 1), (300-n, 300-n))
                    f (x, y) = xyLookup' (x, y) + xyLookup' (x+1, y+1)
                               + grid ! (x, y+n) + grid ! (x+n, y) - xyLookup'' (x+1,y+1)

part1 :: Int -> (Int, Int)
part1 = fst . (!! 2) . maxPartialSums . powerLevels

part2 :: Int -> (Int, Int, Int)
part2 = f . maximumBy (comparing (snd . fst)) . (`zip` [1..300])
        . maxPartialSums . powerLevels
    where f (((x, y), _), n) = (x, y, n)
