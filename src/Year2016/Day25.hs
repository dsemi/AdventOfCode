module Year2016.Day25
    ( part1
    , part2
    ) where

import Year2016.Assembunny
import qualified Year2016.Day08 as D8

import Control.Lens
import Data.Char (chr)
import Data.List (findIndex)


part1 :: String -> Maybe Int
part1 input = findIndex outputMatches [0..]
    where sim = parseInstructions input
          outputMatches x = and $ zipWith (==) (cycle [0, 1])
                            $ take 10 $ evaluateOutput $ a .~ x $ sim

part2 :: String -> IO String
part2 _ = D8.part2 . map chr . evaluateOutput . parseInstructions
          <$> readFile "inputs/2016/bonuschallenge.txt"
