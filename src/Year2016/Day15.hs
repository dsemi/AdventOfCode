module Year2016.Day15
    ( part1
    , part2
    ) where

import Data.Maybe (mapMaybe)
import Text.Megaparsec (digitChar, parseMaybe, some, string)
import Text.Megaparsec.Lexer (integer)
import Text.Megaparsec.String

parser :: Parser (Int, Int)
parser = do
  string "Disc #" >> some digitChar >> string " has "
  ps <- int <* string " positions; at time=0, it is at position "
  p <- int <* string "."
  return (p, ps)
    where int = fromInteger <$> integer

findTarget ds = go 0 ds
    where go c ds
              | map fst ds == target = c
              | otherwise            = go (c+1) $ nextState ds
          target = zipWith (\i (_, ps) -> i `mod` ps) [-1, -2 ..] ds
          nextState = map (\(p, ps) -> ((p + 1) `mod` ps, ps))

part1 :: String -> Int
part1 = findTarget . mapMaybe (parseMaybe parser) . lines

part2 :: String -> Int
part2 = findTarget . mapMaybe (parseMaybe parser) . (++extra) . lines
    where extra = ["Disc #7 has 11 positions; at time=0, it is at position 0."]
