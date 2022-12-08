module Year2022.Day07
    ( part1
    , part2
    ) where

import Data.Char
import Data.List (isPrefixOf)

allSizes :: String -> [Int]
allSizes = go [0] . lines
    where go fstree (line : rest)
              | "$ cd " `isPrefixOf` line = cd (drop 5 line)
              | isDigit (head line) = let size = read (head (words line))
                                          (a:bs) = fstree
                                      in go (a+size:bs) rest
              | otherwise = go fstree rest
              where cd "/" = let sizes = scanl1 (+) fstree
                             in init sizes ++ go [last sizes] rest
                    cd ".." = let (a:b:cs) = fstree
                              in a : go (a+b:cs) rest
                    cd _ = go (0:fstree) rest
          go fstree [] = scanl1 (+) fstree

part1 :: String -> Int
part1 = sum . filter (<=100000) . allSizes

part2 :: String -> Int
part2 input = minimum $ filter (>= target) sizes
    where sizes = allSizes input
          target = last sizes - 40000000
