module Year2016.Day08
    ( part1
    , part2
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.HashSet (HashSet)
import qualified Data.HashSet as S
import Data.List (foldl')
import FlatParse.Basic

import Ocr
import Scanf

type Coord = (Int, Int)

w :: Int
w = 50
h :: Int
h = 6

parseRect :: Parser () (HashSet Coord -> HashSet Coord)
parseRect = do
  (width :+ height :+ ()) <- [fmt|rect %dx%d|]
  pure $ S.union $ S.fromList [(r, c) | c <- [0..width-1], r <- [0..height-1]]

parseRotateRow :: Parser () (HashSet Coord -> HashSet Coord)
parseRotateRow = do
  (row :+ amt :+ ()) <- [fmt|rotate row y=%d by %d|]
  pure $ S.map (\rc@(r, c) -> if r == row then (r, (c + amt) `mod` w) else rc)

parseRotateCol :: Parser () (HashSet Coord -> HashSet Coord)
parseRotateCol = do
  (col :+ amt :+ ()) <- [fmt|rotate column x=%d by %d|]
  pure $ S.map (\rc@(r, c) -> if c == col then ((r + amt) `mod` h, c) else rc)

parser :: Parser () (HashSet Coord -> HashSet Coord)
parser = parseRect <|> parseRotateRow <|> parseRotateCol

litPixels :: ByteString -> HashSet Coord
litPixels = foldl' (flip ($)) S.empty . map (\line -> case runParser parser line of
                                                        OK res _ -> res
                                                        _ -> error "unreachable") . B.lines

part1 :: ByteString -> Int
part1 = S.size . litPixels

showDisplay :: HashSet Coord -> String
showDisplay litPix = '\n' : init (unlines $ map (\r -> map (f . (r,)) [0..w-1]) [0..h-1])
    where f p
              | S.member p litPix = '#'
              | otherwise         = ' '

part2 :: ByteString -> String
part2 = parseLetters . showDisplay . litPixels
