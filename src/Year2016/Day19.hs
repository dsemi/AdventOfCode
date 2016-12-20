{-# LANGUAGE ViewPatterns #-}

module Year2016.Day19
    ( part1
    , part2
    ) where

import Data.Sequence ((|>), Seq, ViewL(..))
import qualified Data.Sequence as S

removeNext :: [Int] -> [Int]
removeNext = go []
    where go c [ ] = reverse c
          go c [x] = x : reverse c
          go c (x:_:xs) = go (x:c) xs

part1 :: Int -> Int
part1 n = head $ until ((==1) . length) removeNext [1..n]

removeAcross :: Seq Int -> Seq Int
removeAcross s@(S.viewl -> (x :< xs)) = S.deleteAt (length s `div` 2 - 1) xs |> x

start :: Seq a -> a
start = (\(l :< _) -> l) . S.viewl

part2 :: Int -> Int
part2 n = start . until ((==1) . length) removeAcross $ S.fromList [1..n]
