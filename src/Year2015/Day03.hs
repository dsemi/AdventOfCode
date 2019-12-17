module Year2015.Day03
    ( part1
    , part2
    ) where

import Control.Arrow
import Data.HashSet (HashSet)
import qualified Data.HashSet as S
import Linear.V2

import Utils


visitedSquares :: String -> HashSet (V2 Int)
visitedSquares = S.fromList . scanl (+) (V2 0 0) . map move

part1 :: String -> Int
part1 = S.size . visitedSquares

everyOther :: [a] -> ([a], [a])
everyOther (x:y:xs) = (x:) *** (y:) $ everyOther xs
everyOther (x:xs) = first (x:) $ everyOther xs
everyOther [] = ([], [])

part2 :: String -> Int
part2 = S.size . uncurry S.union . (visitedSquares *** visitedSquares) . everyOther
