module Year2021.Day03
    ( part1
    , part2
    ) where

import Data.Bits
import Data.Bool
import Data.Char
import Data.List (foldl', partition, transpose)


part1 :: [String] -> Int
part1 input =
    let gamma = foldl' (\a b -> shiftL a 1 .|. b) 0
                . map (\ns -> bool 0 1 $ length (filter (=='1') ns) >= (length ns + 1) `div` 2)
                $ transpose input
    in gamma * ((bit (length $ head input) - 1) `xor` gamma)

mostMatched :: (Int -> Int -> Bool) -> [String] -> Int
mostMatched p = go 0
    where go _  [] = undefined
          go _ [n] = foldl' (\a b -> shiftL a 1 .|. digitToInt b) 0 n
          go i ns = let (a, b) = partition ((=='1') . (!! i)) ns
                    in go (i+1) $ if p (length a) (length b) then a else b

part2 :: [String] -> Int
part2 ns = mostMatched (>=) ns * mostMatched (\a b -> a < b && a /= 0 || b == 0) ns
