{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Year2019.Day10
    ( part1
    , part2
    ) where

import Control.Arrow
import Data.List (maximumBy, sortBy)
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Ord (comparing)


parse :: String -> Set (Int, Int)
parse input = S.fromList $ map fst $ filter ((=='#') . snd)
              $ concatMap (\(y, v) -> zip (map (,y) [0..]) v) $ zip [0..] $ lines input

data Point = Point { cartesian :: (Int, Int)
                   , len :: Double
                   , theta :: Double
                   }

relPoint :: (Int, Int) -> (Int, Int) -> Point
relPoint (x1, y1) (x2, y2)
    | x' >= 0 && y' >= 0 = pt $ pi - atan (x' / y')
    | x' >= 0 && y' <= 0 = pt $ abs $ atan (x' / y')
    | x' < 0 && y' >= 0 = pt $ pi + abs (atan (x' / y'))
    | otherwise = pt $ 2 * pi + atan (x' / y')
    where y' = fromIntegral $ y2 - y1
          x' = fromIntegral $ x2 - x1
          pt = Point (x2, y2) (sqrt (x'^2 + y'^2))

visibilityMap :: (Int, Int) -> Set (Int, Int) -> Map Double [Point]
visibilityMap pt = M.map (sortBy (comparing len)) . M.fromListWith (++)
                   . map ((theta &&& (:[])) . relPoint pt) . S.toList . S.delete pt

maxDetected :: Set (Int, Int) -> Map Double [Point]
maxDetected asts = maximumBy (comparing M.size)
                   $ map (flip visibilityMap asts) $ S.toList asts

part1 :: String -> Int
part1 = M.size . maxDetected . parse

part2 :: String -> String
part2 = show . cartesian . go 199 . maxDetected . parse
    where zap v m
              | length (m ! v) == 1 = M.delete v m
              | otherwise = M.adjust tail v m
          go c m
              | M.null m = error "empty"
              | length keys >= c = head $ m ! (keys !! (c-1))
              | otherwise = go (c - length keys) $ foldr zap m keys
              where keys = M.keys m
