{-# LANGUAGE BangPatterns, NegativeLiterals #-}

module Year2018.Day20
    ( part1
    , part2
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.HashMap.Strict as M
import Linear.V2

parseEdges :: ByteString -> [Int]
parseEdges = M.elems . (\(_, _, x) -> x) . B.foldl' go ([], V2 (0 :: Int) 0, M.empty)
    where go (stack, !pos, !m) c =
              case c of
                '(' -> (pos : stack, pos, m)
                ')' -> (tail stack, head stack, m)
                '|' -> (stack, head stack, m)
                'N' -> move $ V2  0 -1
                'E' -> move $ V2  1  0
                'S' -> move $ V2  0  1
                'W' -> move $ V2 -1  0
                '^' -> (stack, pos, m)
                '$' -> (stack, pos, m)
                _ -> error "unreachable"
              where move dir = let v = M.findWithDefault 0 pos m + 1
                                   pos' = pos + dir
                                   m' = M.insertWith min pos' v m
                               in (stack, pos', m')

part1 :: ByteString -> Int
part1 = maximum . parseEdges

part2 :: ByteString -> Int
part2 = length . filter (>=1000) . parseEdges
