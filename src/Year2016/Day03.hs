module Year2016.Day03
    ( part1
    , part2
    ) where

import Data.Maybe (fromJust)
import Data.List.Split (chunksOf)
import Text.Megaparsec (parseMaybe, space)
import Text.Megaparsec.Lexer (integer)
import Text.Megaparsec.String (Parser)


type Triangle = (Integer, Integer, Integer)

parseTriangles :: String -> [Triangle]
parseTriangles = map (fromJust . parseMaybe parseNums) . lines
    where parseNums :: Parser Triangle
          parseNums = do
            n1 <- space *> integer
            n2 <- space *> integer
            n3 <- space *> integer
            return (n1, n2, n3)

numValidTriangles :: [Triangle] -> Int
numValidTriangles = length . filter isValidTriangle
    where isValidTriangle (a, b, c) = a + b > c && a + c > b && b + c > a

part1 :: String -> String
part1 = show . numValidTriangles . parseTriangles

byCols :: [Triangle] -> [Triangle]
byCols ((a, b, c):(d, e, f):(g, h, i):rest) = (a, d, g) : (b, e, h) : (c, f, i) : byCols rest
byCols [] = []

part2 :: String -> String
part2 = show . numValidTriangles . byCols . parseTriangles
