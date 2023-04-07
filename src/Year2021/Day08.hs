{-# LANGUAGE OverloadedStrings #-}

module Year2021.Day08
    ( part1
    , part2
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.HashMap.Strict as M
import Data.List (elemIndex, foldl1')
import Data.Maybe

import Utils

freqs :: [Int]
freqs = [42, 17, 34, 39, 30, 37, 41, 25, 49, 45];

parse :: ByteString -> [[Int]]
parse = map go . B.lines
    where go :: ByteString -> [Int]
          go line = let [key, ns] = splitOn " | " line
                        hist = B.foldl' (\m c -> M.insertWith (+) c 1 m) M.empty $ B.filter (/=' ') key
                    in map (\n -> let x = B.foldl' (\acc c -> acc + hist M.! c) 0 n
                                  in fromJust $ elemIndex x freqs) $ B.words ns

part1 :: ByteString -> Int
part1 = length . concatMap (filter (`elem` [1, 4, 7, 8])) . parse

part2 :: ByteString -> Int
part2 = sum . map (foldl1' (\a b -> a * 10 + b)) . parse
