{-# LANGUAGE QuasiQuotes #-}

module Year2015.Day05
    ( part1
    , part2
    ) where

import Data.List (group, isInfixOf)
import Text.Regex.PCRE.Heavy


part1 :: String -> Int
part1 = length . filter isNice . lines
    where isNice :: String -> Bool
          isNice s = not (any (`isInfixOf` s) ["ab", "cd", "pq", "xy"])
                     && length (filter (`elem` "aeiou") s) > 2
                     && any ((>=2) . length) (group s)

part2 :: String -> Int
part2 = length . filter isNice2 . lines
    where isNice2 :: String -> Bool
          isNice2 s = s =~ [re|(.)(.).*\1\2|] && s =~ [re|(.).\1|]
