{-# LANGUAGE QuasiQuotes #-}

module Advent.Day11
    ( part1
    , part2
    ) where

import Advent.Problem

import Data.List (tails)
import Text.Regex.PCRE.Heavy (re, scan)

incrStr :: String -> String
incrStr = reverse . step . reverse
    where step [] = []
          step (x:xs)
              | x == 'z' = 'a' : step xs
              | otherwise = succ x : xs

isValid :: String -> Bool
isValid s = not (any (`elem` s) "iol") && isSuccessive s
            && length (scan [re|(.)\1|] s) > 1
    where isSuccessive = any ordered . windows 3
              where ordered []  = True
                    ordered [_] = True
                    ordered (x:y:xs)
                        | y == succ x = ordered $ y : xs
                        | otherwise   = False
                    windows n = takeWhile ((==n) . length)
                                . map (take n) . tails

part1 :: Problem
part1 = PureS $ head . filter isValid . tail . iterate incrStr

part2 :: Problem
part2 = PureS $ (!! 1) . filter isValid . tail . iterate incrStr
