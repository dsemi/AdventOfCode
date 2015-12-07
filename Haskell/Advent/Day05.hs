module Advent.Day05
    ( part1
    , part2
    ) where

import Data.List (group, isInfixOf)
import Text.Regex.PCRE

part1 :: String -> String
part1 = show . length . filter isNice . lines
    where isNice :: String -> Bool
          isNice s = not (any (`isInfixOf` s) ["ab", "cd", "pq", "xy"])
                     && length (filter (`elem` "aeiou") s) > 2
                     && any ((>=2) . length) (group s)

part2 :: String -> String
part2 = show . length . filter isNice2 .lines
    where isNice2 :: String -> Bool
          isNice2 s = s =~ "(..).*\\1" && s =~ "(.).\\1"
