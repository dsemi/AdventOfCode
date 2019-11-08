module Year2015.Day03
    ( part1
    , part2
    ) where

import Control.Arrow
import Control.Lens
import Data.HashSet (HashSet)
import qualified Data.HashSet as S


dirFun :: Char -> (Int, Int) -> (Int, Int)
dirFun '^' = _2 %~ (+1)
dirFun 'v' = _2 %~ subtract 1
dirFun '>' = _1 %~ (+1)
dirFun '<' = _1 %~ subtract 1
dirFun  _  = error "Invalid character"

visitedSquares :: String -> HashSet (Int, Int)
visitedSquares = S.fromList . scanl (flip ($)) (0,0) . map dirFun

part1 :: String -> Int
part1 = S.size . visitedSquares

everyOther :: [a] -> ([a], [a])
everyOther (x:y:xs) = (x:) *** (y:) $ everyOther xs
everyOther (x:xs) = first (x:) $ everyOther xs
everyOther [] = ([], [])

part2 :: String -> Int
part2 = S.size . uncurry S.union . (both %~ visitedSquares) . everyOther
