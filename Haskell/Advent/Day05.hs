{-# LANGUAGE QuasiQuotes #-}

module Advent.Day05
    ( part1
    , part2
    ) where

import Advent.Problem

import Data.List (group, isInfixOf)
import Text.Regex.PCRE.Heavy (re, (=~))

part1 :: Problem
part1 = Pure $ length . filter isNice . lines
    where isNice :: String -> Bool
          isNice s = not (any (`isInfixOf` s) ["ab", "cd", "pq", "xy"])
                     && length (filter (`elem` "aeiou") s) > 2
                     && any ((>=2) . length) (group s)

part2 :: Problem
part2 = Pure $ length . filter isNice2 .lines
    where isNice2 s = s =~ [re|(..).*\1|] && s =~ [re|(.).\1|]
