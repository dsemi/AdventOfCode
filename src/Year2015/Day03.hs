module Year2015.Day03
    ( part1
    , part2
    ) where

import Control.Arrow
import Control.Lens
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS


dirFun :: Char -> (Int, Int) -> (Int, Int)
dirFun '^' = _2 %~ (+1)
dirFun 'v' = _2 %~ subtract 1
dirFun '>' = _1 %~ (+1)
dirFun '<' = _1 %~ subtract 1

visitedSquares :: String -> HashSet (Int, Int)
visitedSquares = HS.fromList . scanl (flip ($)) (0,0) . map dirFun

part1 :: String -> String
part1 = show . HS.size . visitedSquares

part2 :: String -> String
part2 = show . HS.size . uncurry HS.union . (both %~ visitedSquares) . everyOther
    where everyOther :: [a] -> ([a], [a])
          everyOther (x:y:xs) = (x:) *** (y:) $ everyOther xs
          everyOther (x:xs) = first (x:) $ everyOther xs
          everyOther [] = ([], [])
