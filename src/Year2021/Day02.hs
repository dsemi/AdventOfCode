module Year2021.Day02
    ( part1
    , part2
    ) where

import Data.List (foldl')


run :: String -> (Int, Int, Int)
run = foldl' go (0, 0, 0) . lines
    where go (horz, depth, aim) line =
              let [cmd, ns] = words line
                  n = read ns
              in case cmd of
                 "forward" -> (horz + n, depth + (aim * n), aim)
                 "down" -> (horz, depth, aim + n)
                 "up" -> (horz, depth, aim - n)
                 _ -> error "Bad input"

part1 :: String -> Int
part1 = (\(h, _, d) -> h * d) . run

part2 :: String -> Int
part2 = (\(h, d, _) -> h * d) . run
