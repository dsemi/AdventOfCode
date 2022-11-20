module Year2021.Day12
    ( part1
    , part2
    ) where

import Data.Char
import Data.HashMap.Strict ((!), HashMap)
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import Data.List.Split (splitOn)

parse :: String -> HashMap String [String]
parse = M.fromListWith (++) . concatMap f . lines
    where f line = let [k, v] = splitOn "-" line
                   in [(k, [v]), (v, [k])]

dfs :: Bool -> HashMap String [String] -> Int
dfs double m = go double S.empty "start"
    where go _ _ "end" = 1
          go dbl vis k
              | b && (dbl || k == "start") = 0
              | otherwise = let vis' = S.insert k vis
                            in sum $ map (go (dbl || b) vis') (m ! k)
              where b = all isLower k && S.member k vis

part1 :: String -> Int
part1 = dfs True . parse

part2 :: String -> Int
part2 = dfs False . parse
