module Year2017.Day04
    ( part1
    , part2
    ) where

import Data.List (nub, sort)


part1 :: String -> Int
part1 input = sum [ 1 | passphrase <- lines input
                  , let ps = words passphrase
                  , length ps == length (nub ps) ]

part2 :: String -> Int
part2 input = sum [ 1 | passphrase <- lines input
                  , let ps = map sort $ words passphrase
                  , length ps == length (nub ps) ]
