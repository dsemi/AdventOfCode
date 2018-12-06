module Year2018.Day06
    ( part1
    , part2
    ) where

import Control.Arrow ((&&&))
import qualified Data.HashMap.Strict as M
import Data.Ix (range)
import Data.List.Split (splitOn)


type Coord = (Int, Int)

dist :: Coord -> Coord -> Int
dist (a, b) (c, d) = abs (a - c) + abs (b - d)

parseCoords :: String -> [Coord]
parseCoords = map (f . splitOn ", ") . lines
    where f [a, b] = (read a, read b)
          f _ = error "Error parsing coord"

allCoordsWithin :: Int -> [Coord] -> [Coord]
allCoordsWithin buffer xs = range ((x0, y0), (x1, y1))
    where x0 = minimum (map fst xs) - buffer
          y0 = minimum (map snd xs) - buffer
          x1 = maximum (map fst xs) + buffer
          y1 = maximum (map snd xs) + buffer

findLargestFiniteArea :: [Coord] -> Int
findLargestFiniteArea xs = maximum $ zipWith f (go 0) (go 10)
    where f a b = if a == b then a else 0
          go n = M.elems $ M.fromListWith (+)
                 [ (snd d, 1) | coord <- allCoordsWithin n xs
                 , let dists = map (dist coord &&& id) xs
                 , let d = minimum dists
                 , length (filter ((== fst d) . fst) dists) == 1
                 ]

part1 :: String -> Int
part1 = findLargestFiniteArea . parseCoords

findRegionWithin :: Int -> [Coord] -> Int
findRegionWithin n xs = length $ filter (\x -> sum (map (dist x) xs) < n)
                        $ allCoordsWithin (n `div` length xs) xs

part2 :: String -> Int
part2 = findRegionWithin 10000 . parseCoords
