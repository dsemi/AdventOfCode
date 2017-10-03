module Year2015.Day05
    ( part1
    , part2
    ) where

import Utils

import Control.Monad
import Data.List (group, isInfixOf)
import Data.Either
import Text.Megaparsec (parse, try)
import Text.Megaparsec.Char (anyChar, char)


part1 :: String -> Int
part1 = length . filter isNice . lines
    where isNice :: String -> Bool
          isNice s = not (any (`isInfixOf` s) ["ab", "cd", "pq", "xy"])
                     && length (filter (`elem` "aeiou") s) > 2
                     && any ((>=2) . length) (group s)

part2 :: String -> Int
part2 = length . filter isNice2 . lines
    where isNice2 s = twoDoubles && everyOther
              where findDoubles = do
                      a <- anyChar
                      b <- anyChar
                      void $ searchAll (try (char a >> char b))
                    parser :: Parser ()
                    parser = searchAll (try findDoubles)
                    twoDoubles = isRight $ parse parser "" s
                    findSkip = do
                      a <- anyChar
                      anyChar
                      void $ char a
                    parser' :: Parser ()
                    parser' = searchAll (try findSkip)
                    everyOther = isRight $ parse parser' "" s
