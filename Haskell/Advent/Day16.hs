{-# LANGUAGE QuasiQuotes #-}

module Advent.Day16
    ( part1
    , part2
    ) where

import Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as M
import Text.Regex.PCRE.Heavy (re, scan)

tape :: HashMap String Int
tape = M.fromList [ ("children", 3)
                  , ("cats", 7)
                  , ("samoyeds", 2)
                  , ("pomeranians", 3)
                  , ("akitas", 0)
                  , ("vizslas", 0)
                  , ("goldfish", 5)
                  , ("trees", 3)
                  , ("cars", 2)
                  , ("perfumes", 1)
                  ]

parseLines :: String -> [HashMap String Int]
parseLines s = [ M.fromList [(k1, a1'), (k2, a2'), (k3, a3')]
               | [k1, a1, k2, a2, k3, a3] <- map snd $ scan regex s
               , let a1' = read a1
               , let a2' = read a2
               , let a3' = read a3
               ]
    where regex = [re|Sue \d+: ([a-z]+): (\d+), ([a-z]+): (\d+), ([a-z]+): (\d+)|]

couldMatch :: HashMap String Int -> Bool
couldMatch = all (\(k, v) -> tape ! k == v) . M.toList

couldMatch2 :: HashMap String Int -> Bool
couldMatch2 = all match . M.toList
    where match (k, v)
              | k `elem` ["cats", "trees"]           = tape ! k < v
              | k `elem` ["pomeranians", "goldfish"] = tape ! k > v
              | otherwise                            = tape ! k == v

part1 :: String -> String
part1 = show . fst . head . filter (couldMatch . snd) . zip [1..] . parseLines

part2 :: String -> String
part2 = show . fst . head . filter (couldMatch2 . snd) . zip [1..] . parseLines
