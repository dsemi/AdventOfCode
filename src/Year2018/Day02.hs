module Year2018.Day02
    ( part1
    , part2
    ) where

import Data.Maybe (catMaybes)
import Data.List (group, sort, tails)


part1 :: String -> Int
part1 input = length (filter (any (==2)) letterCounts)
              * length (filter (any (==3)) letterCounts)
    where letterCounts = map (map length . group . sort) $ lines input

part2 :: String -> String
part2 = head . concatMap findAlmostMatching . tails . lines
    where keepCommon xs ys = catMaybes $ zipWith (\x y -> if x == y then Just x else Nothing) xs ys
          findAlmostMatching [] = error "Can't find almost matching IDs"
          findAlmostMatching (x:xs) = filter ((== length x - 1) . length) $ map (keepCommon x) xs
