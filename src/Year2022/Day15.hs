module Year2022.Day15
    ( part1
    , part2
    ) where

import Control.Arrow
import Control.Monad
import Data.IntSet (fromList, toList)
import Data.Ix
import Data.List (sortBy)
import Data.List.Split
import Data.Ord

data Sensor = Sensor Int Int Int

type Interval = (Int, Int)

parse :: String -> ([Sensor], [Interval])
parse input = unzip [ (Sensor sx sy dist, (bx, by)) | line <- lines input
                    , let [sx, sy, bx, by] = map (read . last) $ chunksOf 2 $ splitOneOf "=,:" line
                    , let dist = abs (sx - bx) + abs (sy - by) ]

part1 :: String -> Int
part1 input = sum (map rangeSize compressed) -
              length (filter (\x -> any (`inRange` x) compressed) $ toList beaconXs)
    where (sensors, beacons) = parse input
          target = 2000000
          beaconXs = fromList $ map fst $ filter ((== target) . snd) beacons
          intervals = sortBy (comparing fst) [ (x - diff, x + diff) | (Sensor x y dist) <- sensors
                                             , let diff = dist - abs (y - target), diff >= 0 ]
          compressed = go (head intervals) (tail intervals)
              where go int [] = [int]
                    go int@(a0, b0) (int'@(a, b):ints)
                        | a <= b + 1 = go (min a0 a, max b0 b) ints
                        | otherwise = int : go int' ints

data Line = Line Int Int Int Int

intersect :: Line -> Line -> Maybe (Int, Int)
intersect (Line asx asy aex aey) (Line bsx bsy bex bey) = do
  guard $ asx <= bex && bsx <= aex && asy <= bsy && bey <= aey
  let (p1, p2) = (bsx + bsy, asx - asy)
  Just ((p1 + p2) `div` 2, (p1 - p2) `div` 2)

part2 :: String -> Int
part2 input = head [ 4000000*x + y | a <- urs, b <- drs
                   , let (x, y) = maybe (-1, -1) id $ intersect a b
                   , x >= 0 && x <= 4000000 && y >= 0 && y <= 4000000
                   , all (\(Sensor sx sy dist) -> abs (sx - x) + abs (sy - y) > dist) sensors ]
    where sensors = fst $ parse input
          (urs, drs) = (concat *** concat) $ unzip
                       [ ( [ Line (x - dist - 1) y x (y + dist + 1), Line x (y - dist - 1) (x + dist + 1) y ]
                         , [ Line x (y + dist + 1) (x + dist + 1) y, Line (x - dist - 1) y x (y - dist - 1) ] )
                       | (Sensor x y dist) <- sensors]
