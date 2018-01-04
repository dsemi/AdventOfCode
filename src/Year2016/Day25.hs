module Year2016.Day25
    ( part1
    , part2
    ) where

import Year2016.Assembunny
import qualified Year2016.Day08 as D8

import Control.Lens
import Data.Char (chr)
import Data.List (findIndex)
import Data.Maybe (fromJust)
import Pipes
import qualified Pipes.Prelude as P


part1 :: String -> Int
part1 input = fromJust $ findIndex outputMatches [0..]
    where sim = parseInstructions input
          outputMatches x = and $ zipWith (==) (cycle [0, 1])
                            $ P.toList $ evaluateOutput sim' >-> P.take 10
              where sim' = (regs . ix 'a') .~ x $ sim

part2 :: String -> IO String
part2 _ =
  readFile "inputs/2016/bonuschallenge.txt"
  >>= pure . D8.part2 . map chr . P.toList . evaluateOutput . parseInstructions
