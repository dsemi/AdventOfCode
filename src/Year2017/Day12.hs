module Year2017.Day12
    ( part1
    , part2
    ) where

import Utils

import Data.List (foldl')
import Data.List.Split (splitOn)
import Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S


parse :: String -> HashMap Int [Int]
parse input = M.fromList [ (read a, map read $ splitOn ", " b)
                         | [a, b] <- map (splitOn " <-> ") $ lines input ]

part1 :: String -> Int
part1 x = length $ bfs 0 (parse x !)

part2 :: String -> Int
part2 x = fst $ foldl' f (0, S.empty) $ M.keys m
    where m = parse x
          f (c, set) n
              | S.member n set = (c, set)
              | otherwise = (c+1, S.union set $ S.fromList $ map snd $ bfs n (m !))
