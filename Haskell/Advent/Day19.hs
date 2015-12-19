{-# LANGUAGE QuasiQuotes #-}

module Advent.Day19
    ( part1
    , part2
    ) where

import Advent.Problem

import Data.HashSet (HashSet)
import qualified Data.HashSet as S
import Data.List (intercalate)
import Data.Maybe
import Data.String.Utils
import Text.Regex.PCRE.Heavy (re, scan)

parseMapping :: String -> (String, String)
parseMapping s = (k, v)
    where regex = [re|(\w+) => (\w+)|]
          [k, v] = snd . head $ scan regex s

-- E.g. singleReplacements "aa" "xx" "abskaalkjdsaajlkdaa" ->
--   ["abskxxlkjdsaajlkdaa", "abskaalkjdsxxjlkdaa", "abskaalkjdsaajlkdxx"]
singleReplacements :: String -> String -> String -> [String]
singleReplacements k v src = map (intercalate k) parts
    where pieces = split k src
          parts = [ snd $ foldr (\p (i, s) ->
                                     ( i-1
                                     , if i == 0
                                       then (p ++ v ++ head s) : tail s
                                       else p : s
                                     )) (i, []) pieces
                  | i <- [ 1 .. length pieces - 1 ]
                  ]

uniqueSubs :: [(String, String)] -> String -> HashSet String
uniqueSubs reps src = S.fromList $ concat [ singleReplacements k v src | (k, v) <- reps]

uniquePredecessors :: [(String, String)] -> String -> HashSet String
uniquePredecessors reps src = S.fromList $ concat [ singleReplacements v k src | (k, v) <- reps]

findPathToElectron :: [(String, String)] -> String -> Int
findPathToElectron reps = fromJust . go 0
    where go c [] = Nothing
          go c "e" = Just c
          go c s = listToMaybe . mapMaybe (go (c+1))
                   . S.toList $ uniquePredecessors reps s

p1 :: String -> Int
p1 input = let (s:_:mappings) = reverse $ lines input
               reps = map parseMapping mappings
           in S.size $ uniqueSubs reps s

part1 :: Problem
part1 = Pure p1

p2 :: String -> Int
p2 input = let (s:_:mappings) = reverse $ lines input
               reps = map parseMapping mappings
           in findPathToElectron reps s

part2 :: Problem
part2 = Pure p2
