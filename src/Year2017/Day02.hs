module Year2017.Day02
    ( part1
    , part2
    ) where

import Control.Monad
import qualified Control.Foldl as L
import Data.Maybe


parse :: String -> [[Int]]
parse = map (map read . words) . lines

part1 :: String -> Int
part1 = sum . map f . parse
    where f = fromJust . L.fold (liftM2 (-) <$> L.maximum <*> L.minimum)

part2 :: String -> Int
part2 = sum . map f . parse
    where f xs = head [ x `div` y | x <- xs, y <- xs, x /= y, x `mod` y == 0 ]
