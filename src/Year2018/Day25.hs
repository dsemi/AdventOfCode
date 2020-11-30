module Year2018.Day25
    ( part1
    , part2
    ) where

import Control.Lens
import Data.Maybe
import Data.List (partition)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer


type Point = (Int, Int, Int, Int)

parsePoints :: String -> [Point]
parsePoints = fromJust . parseMaybe @() ((do
                [a, b, c, d] <- signed (pure ()) decimal `sepBy` char ','
                pure (a, b, c, d)) `sepBy` newline)

dist :: Point -> Point -> Int
dist (w0, x0, y0, z0) (w1, x1, y1, z1) =
    abs (w0 - w1) + abs (x0 - x1) + abs (y0 - y1) + abs (z0 - z1)

constellations :: [Point] -> [[Point]]
constellations [] = []
constellations pts = let (constellation, rest) = go [head pts] (tail pts)
                     in constellation : constellations rest
    where go ps [] = (ps, [])
          go [] rest = ([], rest)
          go ps ps' =
              let (neighbs, rest) = partition (\p -> any ((<= 3) . dist p) ps) ps'
              in over _1 (ps ++) $ go neighbs rest

part1 :: String -> Int
part1 = length . constellations . parsePoints

part2 :: String -> String
part2 = const ""
