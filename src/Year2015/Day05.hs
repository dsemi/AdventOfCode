{-# LANGUAGE QuasiQuotes #-}

module Year2015.Day05
    ( part1
    , part2
    ) where

import Control.Lens (has)
import Control.Lens.Regex.Text (regex)
import Data.List (group, isInfixOf)
import Data.Text (Text)
import qualified Data.Text as T


part1 :: String -> Int
part1 = length . filter isNice . lines
    where isNice :: String -> Bool
          isNice s = not (any (`isInfixOf` s) ["ab", "cd", "pq", "xy"])
                     && length (filter (`elem` "aeiou") s) > 2
                     && any ((>=2) . length) (group s)

part2 :: Text -> Int
part2 = length . filter isNice2 . T.lines
    where isNice2 :: Text -> Bool
          isNice2 s = has [regex|(.)(.).*\1\2|] s && has [regex|(.).\1|] s
