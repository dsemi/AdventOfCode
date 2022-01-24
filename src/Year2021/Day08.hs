module Year2021.Day08
    ( part1
    , part2
    ) where

import qualified Data.HashMap.Strict as M
import Data.List (elemIndex, foldl1')
import Data.List.Split (splitOn)
import Data.Maybe

freqs :: [Int]
freqs = [42, 17, 34, 39, 30, 37, 41, 25, 49, 45];

parse :: String -> [[Int]]
parse = map go . lines
    where go line = let [key, ns] = splitOn " | " line
                        hist = M.fromListWith (+) $ map (,1) $ filter (/=' ') key
                    in map (\n -> let x = sum $ map (hist M.!) n
                                  in fromJust $ elemIndex x freqs) $ words ns

part1 :: String -> Int
part1 = length . concatMap (filter (`elem` [1, 4, 7, 8])) . parse

part2 :: String -> Int
part2 = sum . map (foldl1' (\a b -> a * 10 + b)) . parse
