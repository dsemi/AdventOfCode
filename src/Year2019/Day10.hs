{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Year2019.Day10
    ( part1
    , part2
    ) where

import Control.Arrow
import Data.List (delete, maximumBy, sortBy)
import qualified Data.Map.Strict as M
import Data.Ord (comparing)
import Linear.V2


parse :: String -> [V2 Int]
parse input = [ V2 x y | (y, line) <- zip [0..] $ lines input
              , (x, v) <- zip [0..] line
              , v == '#'
              ]

theta :: V2 Int -> V2 Int -> Double
theta v1 v2 = atan2 (-x) y
    where V2 x y = fmap fromIntegral $ v2 - v1

dist :: V2 Int -> V2 Int -> Int
dist v1 v2 = sum $ abs $ v2 - v1

visibilities :: V2 Int -> [V2 Int] -> [[V2 Int]]
visibilities pt = map (sortBy $ comparing $ dist pt) . M.elems .
                  M.fromListWith (++) . map (theta pt &&& (:[])) . delete pt

maxDetected :: [V2 Int] -> [[V2 Int]]
maxDetected asts = maximumBy (comparing $ length) $ map (`visibilities` asts) asts

part1 :: String -> Int
part1 = length . maxDetected . parse

part2 :: String -> Int
part2 = (\(V2 a b) -> 100 * a + b) . go 200 . maxDetected . parse
    where go c m
              | null m = error "empty"
              | length m >= c = head $ m !! (c-1)
              | otherwise = go (c - length m)
                            $ filter (not . null) $ map tail $ m
