module Year2016.Day25
    ( part1
    , part2
    ) where

import Year2016.Assembunny

import Control.Lens (set, view)
import Data.Foldable (toList)
import Data.List (findIndex)
import Data.Maybe (fromJust)


part1 :: String -> Int
part1 s = fromJust . findIndex outputMatches
          $ map (\x -> evaluateUntilOutputLengthIs 10 $ set a x sim) [0..]
    where sim = parseInstructions s
          outputMatches = and . zipWith (==) (cycle [0, 1]) . toList . view output

part2 :: String -> String
part2 = const ""
