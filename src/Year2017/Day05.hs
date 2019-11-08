module Year2017.Day05
    ( part1
    , part2
    ) where

import Control.Monad.ST
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as M


calcNumSteps :: (Int -> Int) -> String -> Int
calcNumSteps f input = runST $ V.thaw nums >>= goNext 0 0
    where nums = V.fromList $ map read $ lines input
          goNext c i v
            | i < 0 || i >= M.length v = pure c
            | otherwise = do
                val <- M.read v i
                M.write v i $ f val
                goNext (c + 1) (i + val) v

part1 :: String -> Int
part1 = calcNumSteps (+1)

part2 :: String -> Int
part2 = calcNumSteps (\x -> if x >= 3 then x - 1 else x + 1)
