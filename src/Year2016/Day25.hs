module Year2016.Day25
    ( part1
    , part2
    ) where

import Year2016.Assembunny
import qualified Year2016.Day08 as D8

import Control.Lens
import Data.Char (chr)
import Data.Conduit
import qualified Data.Conduit.List as L
import Data.List (findIndex)
import Data.Maybe (fromJust)


part1 :: String -> Int
part1 input = fromJust . findIndex outputMatches $ map getOutput [0..]
    where sim = parseInstructions input
          getOutput x = runIdentity $ evaluateOutput ((regs . ix 'a') .~ x $ sim) $$ L.take 10
          outputMatches = and . zipWith (==) (cycle [0, 1])

part2 :: String -> IO String
part2 _ =
  readFile "inputs/2016/bonuschallenge.txt"
  >>= pure . D8.part2 . map chr . runIdentity . ($$ L.consume) . evaluateOutput . parseInstructions
