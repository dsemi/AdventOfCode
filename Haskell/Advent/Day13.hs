{-# LANGUAGE QuasiQuotes #-}

module Advent.Day13
    ( part1
    , part2
    ) where

import Advent.Problem

import Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as M
import Data.List (foldl', permutations)
import Data.Maybe
import Text.Regex.PCRE.Heavy (re, scan)

data Edge = Edge String String Int

parseLine :: String -> Edge
parseLine line = let [p1, op, hap, p2] = snd . head $ scan regex line
                     op' = if op == "lose" then negate else id
                 in Edge p1 p2 . op' $ read hap
    where regex = [re|(\S+) would (lose|gain) (\d+) .* (\S+)\.|]


constructMap :: [Edge] -> HashMap String (HashMap String Int)
constructMap = foldl' addEdgeToMap M.empty
    where addEdgeToMap m (Edge p1 p2 n) = let m' = fromMaybe M.empty $ M.lookup p1 m
                                          in M.insert p1 (M.insert p2 n m') m

maxHappinessOrdering :: HashMap String (HashMap String Int) -> Int
maxHappinessOrdering m = maximum $ map (\p -> happinessDiff (head p) (last p)
                                              + sum (zipWith happinessDiff p $ tail p)) orders
    where orders = permutations $ M.keys m
          happinessDiff a b = m ! a ! b + m ! b ! a

part1 :: Problem
part1 = Pure $ maxHappinessOrdering . constructMap . map parseLine . lines

p2 :: String -> Int
p2 input = let m = constructMap . map parseLine $ lines input
               meMap = M.fromList . zip (M.keys m) $ repeat 0
               m' = M.insert "me" meMap $ M.map (M.insert "me" 0) m
           in maxHappinessOrdering m'

part2 :: Problem
part2 = Pure p2
