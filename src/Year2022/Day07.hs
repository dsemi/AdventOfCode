module Year2022.Day07
    ( part1
    , part2
    ) where

import Data.Char
import Data.List (isPrefixOf)

allSizes :: String -> [Int]
allSizes = go 0 [] . tail . lines
    where go size fstree (line : rest)
              | "$ cd " `isPrefixOf` line = cd (drop 5 line)
              | isDigit (head line) = go (size + read (head (words line))) fstree rest
              | otherwise = go size fstree rest
              where cd ".." = case fstree of
                                f : fs -> size : go (size + f) fs rest
                                [] -> error "Tried to go up from root"
                    cd _ = go 0 (size : fstree) rest
          go size fstree [] = scanl (+) size fstree

part1 :: String -> Int
part1 = sum . filter (<=100000) . allSizes

part2 :: String -> Int
part2 input = minimum $ filter (>= target) sizes
    where sizes = allSizes input
          target = last sizes - 40000000
