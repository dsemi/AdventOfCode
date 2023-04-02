module Year2016.Day03
    ( part1
    , part2
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Char
import FlatParse.Basic

type Triangle = (Int, Int, Int)

parseTriangles :: ByteString -> [Triangle]
parseTriangles = map (\line -> case runParser parseNums line of
                                 OK res _ -> res
                                 _ -> error "unreachable") . B.lines
    where parseNums = do
            n1 <- skipSome (satisfy isSpace) *> anyAsciiDecimalInt
            n2 <- skipSome (satisfy isSpace) *> anyAsciiDecimalInt
            n3 <- skipSome (satisfy isSpace) *> anyAsciiDecimalInt
            pure (n1, n2, n3)

numValidTriangles :: [Triangle] -> Int
numValidTriangles = length . filter isValidTriangle
    where isValidTriangle (a, b, c) = a + b > c && a + c > b && b + c > a

part1 :: ByteString -> Int
part1 = numValidTriangles . parseTriangles

byCols :: [Triangle] -> [Triangle]
byCols ((a, b, c):(d, e, f):(g, h, i):rest) = (a, d, g) : (b, e, h) : (c, f, i) : byCols rest
byCols [] = []
byCols _ = error "Bad state"

part2 :: ByteString -> Int
part2 = numValidTriangles . byCols . parseTriangles
