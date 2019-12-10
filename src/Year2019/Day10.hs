{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Year2019.Day10
    ( part1
    , part2
    ) where

import Control.Arrow
import Data.List (delete, maximumBy, sortBy)
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as M
import Data.Ord (comparing)


parse :: String -> [(Int, Int)]
parse input = [ (x, y) | (y, line) <- zip [0..] $ lines input
              , (x, v) <- zip [0..] line
              , v == '#'
              ]

data Point = Point { cartesian :: (Int, Int)
                   , len :: Double
                   , theta :: Double
                   }

-- Polar coord starting at (0, -1) (where -y is up) and rotating clockwise
relPoint :: (Int, Int) -> (Int, Int) -> Point
relPoint (x1, y1) (x2, y2)
    | x' >= 0 && y' >= 0 = pt $ pi - atan (x' / y')
    | x' >= 0 && y' < 0 = pt $ abs (atan (x' / y'))
    | x' < 0 && y' >= 0 = pt $ pi + abs (atan (x' / y'))
    | otherwise = pt $ 2 * pi - atan (x' / y')
    where y' = fromIntegral $ y2 - y1
          x' = fromIntegral $ x2 - x1
          pt = Point (x2, y2) (sqrt (x'^2 + y'^2))

visibilityMap :: (Int, Int) -> [(Int, Int)] -> Map Double [Point]
visibilityMap pt = M.map (sortBy (comparing len)) . M.fromListWith (++)
                   . map ((theta &&& (:[])) . relPoint pt) . delete pt

maxDetected :: [(Int, Int)] -> Map Double [Point]
maxDetected asts = maximumBy (comparing M.size) $ map (`visibilityMap` asts) asts

part1 :: String -> Int
part1 = M.size . maxDetected . parse

part2 :: String -> Int
part2 = (\(a, b) -> 100 * a + b). cartesian . go 200 . maxDetected . parse
    where zap v m
              | length (m ! v) == 1 = M.delete v m
              | otherwise = M.adjust tail v m
          go c m
              | M.null m = error "empty"
              | length keys >= c = head $ m ! (keys !! (c-1))
              | otherwise = go (c - length keys) $ foldr zap m keys
              where keys = M.keys m
