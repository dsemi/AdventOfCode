{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Year2020.Day22
    ( part1
    , part2
    ) where

import Data.Either
import Data.List.Split
import qualified Data.Set as S


parse :: String -> ([Int], [Int])
parse input = let [a, b] = splitOn "\n\n" input
              in (f a, f b)
    where f = map read . tail . lines

play :: Bool -> [Int] -> [Int] -> Either Int Int
play p2 = go False S.empty
    where go _ _ as [] = Left $ sum $ zipWith (*) [1..] $ reverse as
          go _ _ [] bs = Right $ sum $ zipWith (*) [1..] $ reverse bs
          go sub s (a:as) (b:bs)
              | sub && maximum (a:as) > maximum (b:bs) = Left undefined
              | p2 && S.member (a:as, b:bs) s = Left undefined
              | p1Wins = go sub s' (as ++ [a, b]) bs
              | otherwise = go sub s' as (bs ++ [b, a])
              where s' = S.insert (a:as, b:bs) s
                    p1Wins = if p2 && a <= length as && b <= length bs
                             then isLeft $ go True S.empty (take a as) (take b bs)
                             else a > b

part1 :: String -> Either Int Int
part1 = uncurry (play False) . parse

part2 :: String -> Either Int Int
part2 = uncurry (play True) . parse
