module Year2019.Day03
    ( part1
    , part2
    ) where

import Data.List.Split (splitOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import DaysTH


$(buildProb)

parseWire :: String -> Map (Int, Int) Int
parseWire = M.fromListWith min . flip zip [1..] . tail
            . scanl move (0, 0) . concatMap expandSteps . splitOn ","
    where expandSteps (d:ds) = replicate (read ds) d
          expandSteps [] = error "Unable to parse instr"
          move (x, y) 'U' = (x, y+1)
          move (x, y) 'D' = (x, y-1)
          move (x, y) 'L' = (x-1, y)
          move (x, y) 'R' = (x+1, y)
          move _      _   = error "Unknown direction"

part1' :: String -> Int
part1' = minimum . map (\(x, y) -> abs x + abs y) . M.keys
         . foldr1 M.intersection . map parseWire . lines

part2' :: String -> Int
part2' = minimum . M.elems . foldr1 (M.intersectionWith (+)) . map parseWire . lines
