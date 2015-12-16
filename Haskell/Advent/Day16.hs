{-# LANGUAGE QuasiQuotes #-}

module Advent.Day16
    ( part1
    , part2
    ) where

import Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as M
import Text.Regex.PCRE.Heavy (re, scan)

tape :: HashMap String (Int -> Bool)
tape = M.fromList [ ("children", (==3))
                  , ("cats", (==7))
                  , ("samoyeds", (==2))
                  , ("pomeranians", (==3))
                  , ("akitas", (==0))
                  , ("vizslas", (==0))
                  , ("goldfish", (==5))
                  , ("trees", (==3))
                  , ("cars", (==2))
                  , ("perfumes", (==1))
                  ]

parseLines :: String -> [HashMap String Int]
parseLines s = [ M.fromList [(k1, a1'), (k2, a2'), (k3, a3')]
               | [k1, a1, k2, a2, k3, a3] <- map snd $ scan regex s
               , let a1' = read a1
               , let a2' = read a2
               , let a3' = read a3
               ]
    where regex = [re|Sue \d+: ([a-z]+): (\d+), ([a-z]+): (\d+), ([a-z]+): (\d+)|]

couldMatch :: HashMap String (Int -> Bool) -> HashMap String Int -> Bool
couldMatch tape = all (uncurry (tape !)) . M.toList

solve :: HashMap String (Int -> Bool) -> String -> String
solve tape' = show . fst . head . filter (couldMatch tape' . snd) . zip [1..] . parseLines

part1 :: String -> String
part1 = solve tape

part2 :: String -> String
part2 = solve tape'
    where tape' = M.fromList [ ("cats", (>7))
                             , ("pomeranians", (<3))
                             , ("goldfish", (<5))
                             , ("trees", (>3))
                             ] `M.union` tape
