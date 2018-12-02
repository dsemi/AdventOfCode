module Year2018.Day02
    ( part1
    , part2
    ) where

import Data.List (group, sort, tails)


part1 :: String -> Int
part1 input = length (filter (any (==2)) letterCounts)
              * length (filter (any (==3)) letterCounts)
    where letterCounts = map (map length . group . sort) $ lines input


part2 :: String -> String
part2 = head . concatMap findAlmostMatching . tails . lines
    where keepCommon [] _ = []
          keepCommon _ [] = []
          keepCommon (x:xs) (y:ys)
              | x == y = x : keepCommon xs ys
              | otherwise = keepCommon xs ys
          findAlmostMatching [] = error "Can't find almost matching IDs"
          findAlmostMatching (x:xs) = filter ((== length x - 1) . length) $ map (keepCommon x) xs
