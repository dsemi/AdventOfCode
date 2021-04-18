module Year2020.Day01
    ( part1
    , part2
    ) where

import qualified Data.IntSet as S


solve :: Int -> String -> Int
solve x = head . go x 2020 . S.fromList . map read . lines
    where go 1 c xs
              | c `S.member` xs = [c]
              | otherwise = []
          go n c xs = [ x2*x3 | x2 <- S.toList xs
                      , x3 <- go (n-1) (c-x2) (snd (S.split x2 xs)) ]

part1 :: String -> Int
part1 = solve 2

part2 :: String -> Int
part2 = solve 3
