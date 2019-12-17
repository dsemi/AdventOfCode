module Year2019.Day16
    ( part1
    , part2
    ) where

import Data.Char
import qualified Data.Vector.Unboxed as V


applyPattern :: Int -> [Int]
applyPattern n = tail $ concatMap (replicate n) $ cycle [0, 1, 0, -1]

phase :: Int -> [Int] -> String
phase n = map intToDigit . (!! n) . iterate go
    where go ns = zipWith (\i _ -> abs (sum (zipWith (*) (applyPattern i) ns)) `rem` 10) [1..] ns

part1 :: String -> String
part1 = take 8 . phase 100 . map digitToInt

solve :: Int -> Int -> [Int] -> String
solve n offset (concat . replicate 10000 -> digits) =
    map intToDigit $ V.toList $ (!! n) $ iterate f $ V.fromList ds
    where ds | offset > length digits `div` 2 = drop offset digits
             | otherwise = error "Offset is not large enough"
          f = V.map ((`rem` 10) . abs) . V.scanr1' (+)

part2 :: String -> String
part2 input = take 8 $ solve 100 offset $ map digitToInt input
    where offset = read $ take 7 input
