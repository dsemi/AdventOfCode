module Year2016.Day25
    ( part1
    , part2
    ) where

import Year2016.Assembunny
import qualified Year2016.Day08 as D8

import Conduit
import Control.Lens
import Data.Char (chr)
import qualified Data.Conduit.List as L
import Data.List (findIndex)
import Data.Maybe (fromJust)


part1 :: String -> Int
part1 input = fromJust $ findIndex outputMatches [0..]
    where sim = parseInstructions input
          outputMatches x = and $ zipWith (==) (cycle [0, 1])
                            $ runConduitPure $ evaluateOutput sim' .| L.take 10
              where sim' = a .~ x $ sim

part2 :: String -> IO String
part2 _ = D8.part2 . map chr . runConduitPure . (.| L.consume) . evaluateOutput . parseInstructions
          <$> readFile "inputs/2016/bonuschallenge.txt"
