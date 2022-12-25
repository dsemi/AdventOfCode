module Year2022.Day25
    ( part1
    , part2
    ) where

import Data.List (elemIndex)
import Data.Maybe

(!) :: (Eq a) => [a] -> a -> Int
(!) xs x = fromJust $ elemIndex x xs

part1 :: String -> String
part1 = reverse . g . sum . map (f . reverse) . lines
    where f line = sum [ 5^i * ("=-012" ! c - 2) | (i, c) <- zip [0..] line]
          g 0 = []
          g n = "012=-" !! (n `mod` 5) : g ((n + 2) `div` 5)

part2 :: String -> String
part2 = const " "
