module Year2022.Day15
    ( part1
    , part2
    ) where

import Control.Arrow
import Control.Monad
import Data.IntSet (IntSet, fromList, toList)
import Data.List.Split

data Sensor = Sensor Int Int Int

data Interval = Interval Int Int

union :: Interval -> Interval -> Interval
union (Interval lo0 hi0) (Interval lo1 hi1) = Interval (min lo0 lo1) (max hi0 hi1)

len :: Interval -> Int
len (Interval lo hi) = hi - lo

parse :: String -> ([Sensor], IntSet)
parse input = (sens, beaconXs)
    where (sens, beacs) = unzip [ (Sensor sx sy dist, (bx, by)) | line <- lines input
                                , let [sx, sy, bx, by] = map (read . last) $ chunksOf 2 $ splitOneOf "=,:" line
                                , let dist = abs (sx - bx) + abs (sy - by) ]
          beaconXs = fromList $ map fst $ filter ((== 2000000) . snd) beacs

part1 :: String -> Int
part1 input = len interval - length (filter (\b -> b >= lo && b < hi) $ toList bs)
    where (sensors, bs) = parse input
          interval@(Interval lo hi) = foldl1 union [ Interval (x - diff) (x + diff + 1)
                                                   | (Sensor x y dist) <- sensors
                                                   , let diff = dist - abs (y - 2000000)
                                                   , diff >= 0 ]

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
