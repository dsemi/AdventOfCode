module Advent.Day11
    ( part1
    , part2
    ) where

import Data.Char
import Data.List (isInfixOf, zipWith3)
import Text.Regex.PCRE

incrStr :: String -> String
incrStr = snd . foldr f (1, "")
    where lowerLimit = ord 'a'
          upperLimit = ord 'z' + 1
          f x (c, s) = let code = ord x + c
                       in ( code `div` upperLimit
                          , chr (max lowerLimit $ code `mod` upperLimit) : s)

isValid :: String -> Bool
isValid s = not (any (`elem` s) "iol")
            && any (`isInfixOf` s) (zipWith3 (\a b c -> [a, b, c]) letters (tail letters) (drop 2 letters))
            && length (getAllTextMatches $ s =~ "(.)\\1" :: [String]) > 1
    where letters = ['a'..'z']

part1 :: String -> String
part1 = head . filter isValid . tail . iterate incrStr

part2 :: String -> String
part2 = (!! 1) . filter isValid . tail . iterate incrStr
