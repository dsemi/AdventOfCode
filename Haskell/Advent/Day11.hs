module Advent.Day11
    ( part1
    , part2
    ) where

import Data.List (isInfixOf, zipWith3)
import Text.Regex.PCRE

incrStr :: String -> String
incrStr = snd . foldr (\x (c, s) -> (x == 'z' && c, (if c then incr x else x) : s)) (True, "")
    where incr c = dropWhile (/= c) letters !! 1
          letters = cycle ['a'..'z']

isValid :: String -> Bool
isValid s = not (any (`elem` s) "iol")
            && any (`isInfixOf` s) (zipWith3 (\a b c -> [a, b, c]) letters (tail letters) (drop 2 letters))
            && length (getAllTextMatches $ s =~ "(.)\\1" :: [String]) > 1
    where letters = ['a'..'z']

part1 :: String -> String
part1 = head . filter isValid . tail . iterate incrStr

part2 :: String -> String
part2 = (!! 1) . filter isValid . tail . iterate incrStr
