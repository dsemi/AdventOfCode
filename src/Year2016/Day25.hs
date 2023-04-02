module Year2016.Day25
    ( part1
    , part2
    ) where

import Control.Lens
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Char (chr)
import Data.List (findIndex)

import Year2016.Assembunny
import qualified Year2016.Day08 as D8

part1 :: ByteString -> Maybe Int
part1 input = findIndex outputMatches [0..]
    where sim = parseInstructions input
          outputMatches x = and $ zipWith (==) (cycle [0, 1])
                            $ take 10 $ evaluateOutput $ a .~ x $ sim

part2 :: ByteString -> IO String
part2 _ = D8.part2 . B.pack . map chr . evaluateOutput . parseInstructions
          <$> B.readFile "inputs/2016/bonuschallenge.txt"
