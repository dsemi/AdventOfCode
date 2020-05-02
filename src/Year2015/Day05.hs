module Year2015.Day05
    ( part1
    , part2
    ) where

import Utils

import Data.Either (isRight)
import Data.List (group, isInfixOf)
import Text.Megaparsec (anySingle, parse)
import Text.Megaparsec.Char (char)


part1 :: String -> Int
part1 = length . filter isNice . lines
    where isNice :: String -> Bool
          isNice s = not (any (`isInfixOf` s) ["ab", "cd", "pq", "xy"])
                     && length (filter (`elem` "aeiou") s) > 2
                     && any ((>=2) . length) (group s)

part2 :: String -> Int
part2 = length . filter isNice2 . lines
    where f = searchAll $ do
            (a, b) <- (,) <$> anySingle <*> anySingle
            searchAll (char a >> char b)
          g = searchAll $ anySingle <* anySingle >>= char
          isNice2 :: String -> Bool
          isNice2 s = isRight (parse f "" s) && isRight (parse g "" s)
