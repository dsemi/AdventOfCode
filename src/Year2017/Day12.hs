module Year2017.Day12
    ( part1
    , part2
    ) where

import Data.List (foldl')
import Data.List.Split (splitOn)
import Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as M
import Data.HashSet (HashSet)
import qualified Data.HashSet as S
import Data.Sequence (Seq(..), (><))
import qualified Data.Sequence as Q


parse :: String -> HashMap Int (Seq Int)
parse input = M.fromList [ (read a, Q.fromList $ map read $ splitOn ", " b)
                         | [a, b] <- map (splitOn " <-> ") $ lines input ]

bfs :: HashMap Int (Seq Int) -> Int -> HashSet Int
bfs m = go m S.empty . Q.singleton
    where go m visited Empty = visited
          go m visited (a :<| queue)
              | S.member a visited = go m visited queue
              | otherwise = go m (S.insert a visited) $ queue >< m ! a


part1 :: String -> Int
part1 x = S.size $ bfs (parse x) 0

part2 :: String -> Int
part2 x = fst $ foldl' f (0, S.empty) $ M.keys m
    where m = parse x
          f (c, set) n
              | S.member n set = (c, set)
              | otherwise = (c+1, S.union set $ bfs m n)
