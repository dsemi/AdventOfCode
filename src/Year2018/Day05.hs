module Year2018.Day05
    ( part1
    , part2
    ) where

import Data.Char


react :: String -> Int
react = length . foldr go []
    where go x (y:ys)
              | x /= y && toLower x == toLower y = ys
          go x ys = x : ys

part1 :: String -> Int
part1 = react

part2 :: String -> Int
part2 s = minimum [react $ filter ((/=c) . toLower) s | c <- ['a'..'z']]
