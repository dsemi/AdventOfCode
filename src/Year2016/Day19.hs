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

iterateUntil :: (a -> Bool) -> (a -> a) -> a -> a
iterateUntil cond f x = head . filter cond $ iterate f x

part1 :: String -> Int
part1 s = head $ iterateUntil ((==1) . length) removeNext [1..(read s)]

removeAcross :: Seq Int -> Seq Int
removeAcross s@(S.viewl -> (x :< xs)) = S.deleteAt (length s `div` 2 - 1) xs |> x

start :: Seq a -> a
start = (\(l :< _) -> l) . S.viewl

part2 :: String -> Int
part2 s = start . iterateUntil ((==1) . length) removeAcross $ S.fromList [1..(read s)]
