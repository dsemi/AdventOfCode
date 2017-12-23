module Year2016.Day25
    ( part1
    , part2
    ) where

import Year2016.Assembunny
import qualified Year2016.Day08 as D8

import Control.Lens
import Data.Char (chr)
import Data.Foldable (toList)
import Data.List (findIndex)
import Data.Maybe (fromJust)


part1 :: String -> Int
part1 s = fromJust . findIndex outputMatches
          $ map (\x -> evaluateUntilOutputLengthIs 10 $ (regs . ix 'a') .~ x $ sim) [0..]
    where sim = parseInstructions s
          outputMatches = and . zipWith (==) (cycle [0, 1]) . toList . view output

part2 :: String -> IO String
part2 _ =
  readFile "inputs/2016/bonuschallenge.txt"
  >>= return . D8.part2 . map chr . toList . view output . evaluate . parseInstructions
