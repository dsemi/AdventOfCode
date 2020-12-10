module Year2020.Day10
    ( part1
    , part2
    ) where

import Data.Function.Memoize
import Data.List (sort)


parse :: String -> [Int]
parse input = 0 : (ns ++ [maximum ns + 3])
    where ns = sort $ map read $ lines input

joltz :: [Int] -> Int
joltz ns = length (filter (==1) js) * length (filter (==3) js)
    where js = zipWith (-) (tail ns) (ns)

part1 :: String -> Int
part1 = joltz . parse

numValid :: [Int] -> Int
numValid ns = memoFix go $ last ns
    where go f n
              | n < 0 || n `notElem` ns = 0
              | n == 0 = 1
              | otherwise = f (n-1) + f (n-2) + f (n-3)

part2 :: String -> Int
part2 = numValid . parse
