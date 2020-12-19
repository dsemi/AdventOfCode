{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Year2020.Day19
    ( part1
    , part2
    ) where

import Data.List.Split
import Data.Map (Map, (!))
import qualified Data.Map as M


data Rule = Const Char | Rule [[Int]]

parse :: String -> (Map Int Rule, [String])
parse input = let [rules, inp] = splitOn "\n\n" input
              in ( M.fromList $ map f $ lines rules
                 , lines inp)
    where f x = let [k, v] = splitOn ": " x
                in if head v == '"'
                   then (read k, Const (v !! 1))
                   else (read k, Rule $ map (map read . words) $ splitOn " | " v)

matches :: Map Int Rule -> String -> Bool
matches rules = any null . go (rules M.! 0)
    where go :: Rule -> String -> [String]
          go _ [] = []
          go (Const c) (x:xs) = if c == x then [xs] else []
          go (Rule rs) xs = concatMap (foldl (\ys r -> concatMap (go r) ys) [xs] . map (rules !)) rs

part1 :: String -> Int
part1 (parse -> (rules, strs)) = length $ filter (matches rules) strs

part2 :: String -> Int
part2 (parse -> (rules, strs)) = length $ filter (matches rules') strs
    where rules' = M.fromList [ (8, Rule [[42], [42, 8]])
                              , (11, Rule [[42, 31], [42, 11, 31]]) ]
                   `M.union` rules
