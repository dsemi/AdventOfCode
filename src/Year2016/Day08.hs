{-# LANGUAGE TupleSections #-}

module Year2016.Day08
    ( part1
    , part2
    ) where

import Data.HashSet (HashSet)
import qualified Data.HashSet as S
import Data.List (foldl')
import Data.Maybe (mapMaybe)
import Text.Megaparsec ((<|>), parseMaybe, string)
import Text.Megaparsec.Lexer (integer)
import Text.Megaparsec.String (Parser)


type Coord = (Int, Int)

int = fromInteger <$> integer

w = 50
h = 6

parseRect :: Parser (HashSet Coord -> HashSet Coord)
parseRect = do
  width <- string "rect " *> int
  height <- string "x" *> int
  return $ S.union $ S.fromList [(r, c) | c <- [0..width-1], r <- [0..height-1]]

parseRotateRow :: Parser (HashSet Coord -> HashSet Coord)
parseRotateRow = do
  row <- string "rotate row y=" *> int
  amt <- string " by " *> int
  return $ S.map (\rc@(r, c) -> if r == row then (r, (c + amt) `mod` w) else rc)

parseRotateCol :: Parser (HashSet Coord -> HashSet Coord)
parseRotateCol = do
  col <- string "rotate column x=" *> int
  amt <- string " by " *> int
  return $ S.map (\rc@(r, c) -> if c == col then ((r + amt) `mod` h, c) else rc)

parser :: Parser (HashSet Coord -> HashSet Coord)
parser = parseRect <|> parseRotateRow <|> parseRotateCol

litPixels :: String -> HashSet Coord
litPixels = foldl' (flip ($)) S.empty . mapMaybe (parseMaybe parser) . lines

part1 :: String -> Int
part1 = S.size . litPixels

showDisplay :: HashSet Coord -> String
showDisplay litPix = '\n' : (unlines $ map (\r -> map (f . (r,)) [0..w-1]) [0..h-1])
    where f p
              | S.member p litPix = '#'
              | otherwise         = ' '

part2 :: String -> String
part2 = showDisplay . litPixels
