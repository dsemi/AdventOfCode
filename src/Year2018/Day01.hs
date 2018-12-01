module Year2018.Day01
    ( part1
    , part2
    ) where

import qualified Data.IntSet as S


readInts :: String -> [Int]
readInts = map (read . filter (/= '+')) . lines

part1 :: String -> Int
part1 = sum . readInts

part2 :: String -> Int
part2 = go S.empty . scanl (+) 0 . cycle . readInts
    where go _ [] = error "No duplicate frequency found"
          go s (x:xs)
              | x `S.member` s = x
              | otherwise = go (S.insert x s) xs
