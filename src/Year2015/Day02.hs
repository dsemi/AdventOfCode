module Year2015.Day02
    ( part1
    , part2
    ) where

import Data.List.Split


process :: (Num a, Read a) => (a -> a -> a -> a) -> String -> a
process f xs = sum [ f l w h | [l, w, h] <- map (map read . splitOn "x") $ lines xs ]

part1 :: String -> Int
part1 = process (\l w h -> 2*l*w + 2*l*h + 2*w*h + minimum [l*w, l*h, w*h])

part2 :: String -> Int
part2 = process (\l w h -> l*w*h + 2 * minimum [l+w, l+h, w+h])
