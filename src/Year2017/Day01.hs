module Year2017.Day01
    ( part1
    , part2
    ) where

import Data.Char (digitToInt)


rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = zipWith const (drop n (cycle xs)) xs

captcha :: Int -> [Int] -> Int
captcha offset xs = sum $ zipWith f xs $ rotate offset xs
    where f a b = if a == b then a else 0

part1 :: String -> Int
part1 = captcha 1 . map digitToInt

part2 :: String -> Int
part2 input = let digits = map digitToInt input
              in captcha (length digits `div` 2) digits
