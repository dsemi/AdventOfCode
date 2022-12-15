module Year2022.Day15
    ( part1
    , part2
    ) where

import Control.Arrow
import Data.IntSet (IntSet)
import qualified Data.IntSet as S
import Data.List (foldl1')
import Data.List.Split
import Data.Maybe
import Linear.V2

data Sensor = Sensor (V2 Int) Int

data Interval = Interval Int Int

union :: Interval -> Interval -> Interval
union (Interval lo0 hi0) (Interval lo1 hi1) = Interval (min lo0 lo1) (max hi0 hi1)

len :: Interval -> Int
len (Interval lo hi) = hi - lo

parse :: String -> ([Sensor], IntSet)
parse input = (sensors, beaconXs)
    where (sensors, beacons) = unzip [ (Sensor sPos dist, bPos) | line <- lines input
                                     , let [_, sx, _, sy, _, bx, _, by] = splitOneOf "=,:" line
                                     , let (sPos, bPos) = (V2 (read sx) (read sy), V2 (read bx) (read by))
                                     , let dist = sum $ abs $ sPos - bPos ]
          beaconXs = S.fromList $ map (\(V2 x _) -> x) $ filter (\(V2 _ y) -> y == 2000000) beacons

part1 :: String -> Int
part1 input = len interval - length (filter (\b -> b >= lo && b < hi) $ S.toList bs)
    where (sensors, bs) = parse input
          y = 2000000
          intervals = [ Interval (x - diff) (x + diff + 1) | (Sensor (V2 x sy) dist) <- sensors
                      , let diff = dist - abs (sy - y)
                      , diff >= 0 ]
          interval@(Interval lo hi) = foldl1' union intervals

data Line = Line (V2 Int) (V2 Int)

intersect :: Line -> Line -> Maybe (V2 Int)
intersect (Line (V2 asx asy) (V2 aex aey)) (Line (V2 bsx bsy) (V2 bex bey))
    | asx <= bex && bsx <= aex && asy <= bsy && bey <= aey =
        let (p1, p2) = (bsx + bsy, asx - asy)
        in Just $ V2 ((p1 + p2) `div` 2) ((p1 - p2) `div` 2)
    | otherwise = Nothing

part2 :: String -> Int
part2 input = head [ 4000000*x + y | a <- urs, b <- drs
                   , pos@(V2 x y) <- maybeToList $ intersect a b
                   , x >= 0 && x <= 4000000 && y >= 0 && y <= 4000000
                   , all (\(Sensor sp dist) -> sum (abs $ pos - sp) > dist) sensors ]
    where sensors = fst $ parse input
          (urs, drs) = (concat *** concat) $ unzip
                       [ ( [ Line (V2 (x - dist - 1) y) (V2 x (y + dist + 1))
                           , Line (V2 x (y - dist - 1)) (V2 (x + dist + 1) y) ]
                         , [ Line (V2 x (y + dist + 1)) (V2 (x + dist + 1) y)
                           , Line (V2 (x - dist - 1) y) (V2 x (y - dist - 1)) ])
                       | (Sensor (V2 x y) dist) <- sensors]
