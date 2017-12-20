module Year2016.Day03
    ( part1
    , part2
    ) where

import Utils

import Data.Maybe (mapMaybe)
import Text.Megaparsec (parseMaybe)
import Text.Megaparsec.Char (space)
import Text.Megaparsec.Char.Lexer (decimal)


type Triangle = (Integer, Integer, Integer)

parseTriangles :: String -> [Triangle]
parseTriangles = mapMaybe (parseMaybe parseNums) . lines
    where parseNums :: Parser Triangle
          parseNums = do
            n1 <- space *> decimal
            n2 <- space *> decimal
            n3 <- space *> decimal
            return (n1, n2, n3)

numValidTriangles :: [Triangle] -> Int
numValidTriangles = length . filter isValidTriangle
    where isValidTriangle (a, b, c) = a + b > c && a + c > b && b + c > a

part1 :: String -> Int
part1 = numValidTriangles . parseTriangles

byCols :: [Triangle] -> [Triangle]
byCols ((a, b, c):(d, e, f):(g, h, i):rest) = (a, d, g) : (b, e, h) : (c, f, i) : byCols rest
byCols [] = []
byCols _ = error "Bad state"

part2 :: String -> Int
part2 = numValidTriangles . byCols . parseTriangles
