module Advent.Day08
    ( part1
    , part2
    ) where

import Data.List (foldl')

part1 :: String -> String
part1 = show . foldl' (\acc line -> let str = parseStr $ init $ tail line
                                    in acc + length line - length str) 0 . lines
    where parseStr ('\\': '\\': xs)      = '\\' : parseStr xs
          parseStr ('\\': '"': xs)       = '"' : parseStr xs
          parseStr ('\\': 'x': _: _: xs) = '_' : parseStr xs
          parseStr (x:xs)                = x : parseStr xs
          parseStr []                    = []

part2 :: String -> String
part2 = show . sum . map ((+2) . sum . map getLen) . lines
    where getLen '\\' = 1
          getLen '"'  = 1
          getLen  _   = 0
