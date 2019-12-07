module Year2019.Day03
    ( part1
    , part2
    ) where

import Data.List.Split (splitOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Linear.V2


parseWire :: String -> Map (V2 Int) Int
parseWire = M.fromListWith min . flip zip [1..]
            . scanl1 (+) . concatMap expandSteps . splitOn ","
    where expandSteps (d:ds) = replicate (read ds) $ move d
          expandSteps [] = error "Unable to parse instr"
          move 'U' = V2 0 1
          move 'D' = V2 0 (-1)
          move 'L' = V2 (-1) 0
          move 'R' = V2 1 0
          move _   = error "Unknown direction"

part1 :: String -> Int
part1 = minimum . map (sum . abs) . M.keys
        . foldr1 M.intersection . map parseWire . lines

part2 :: String -> Int
part2 = minimum . foldr1 (M.intersectionWith (+)) . map parseWire . lines
