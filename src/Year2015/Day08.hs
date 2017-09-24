module Year2015.Day08
    ( part1
    , part2
    ) where

import Data.Functor.Foldable


part1 :: String -> Int
part1 = cata lenDiff . lines
    where parseStr ('\\': '\\': xs)      = '\\' : parseStr xs
          parseStr ('\\': '"': xs)       = '"' : parseStr xs
          parseStr ('\\': 'x': _: _: xs) = '_' : parseStr xs
          parseStr (x:xs)                = x : parseStr xs
          parseStr []                    = []
          lenDiff (Nil) = 0
          lenDiff (Cons line t) = let str = parseStr $ init $ tail line
                                  in t + length line - length str


part2 :: String -> Int
part2 = sum . map ((+2) . sum . map getLen) . lines
    where getLen '\\' = 1
          getLen '"'  = 1
          getLen  _   = 0
