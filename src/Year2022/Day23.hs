module Year2022.Day23
    ( part1
    , part2
    ) where

import Data.HashSet (HashSet)
import qualified Data.HashSet as S
import Data.List (findIndex)
import Linear.V2

dirs :: [V2 Int]
dirs = [V2 (-1) 1, V2 0 1, V2 1 1, V2 (-1) 0, V2 1 0, V2 (-1) (-1), V2 0 (-1), V2 1 (-1)]

move :: Int -> V2 Int -> HashSet (V2 Int) -> V2 Int
move dir elf elves
    | and adjs || null pot = elf
    | otherwise = head pot
    where adjs = [ not (S.member (elf + d) elves) | d <- dirs ]
          poss = [ (adjs !! 0 && adjs !! 1 && adjs !! 2, elf + V2 0 1)
                 , (adjs !! 5 && adjs !! 6 && adjs !! 7, elf + V2 0 (-1))
                 , (adjs !! 0 && adjs !! 3 && adjs !! 5, elf + V2 (-1) 0)
                 , (adjs !! 2 && adjs !! 4 && adjs !! 7, elf + V2 1 0)]
          pot = [ elf' | i <- [0..3]
                , let (avail, elf') = poss !! ((dir + i) `mod` 4)
                , avail ]

steps :: String -> [HashSet (V2 Int)]
steps input = let elves = S.fromList [V2 c (-r) | (r, row) <- zip [0..] $ lines input
                                     , (c, v) <- zip [0..] row
                                     , v == '#' ]
              in elves : go 0 elves
    where go dir elves = next : go ((dir + 1) `mod` 4) next
              where next = S.foldl' f S.empty elves
                    f elves' elf
                        | S.member elf' elves' = S.insert (2*elf' - elf)
                                                 $ S.insert elf
                                                 $ S.delete elf' elves'
                        | otherwise = S.insert elf' elves'
                        where elf' = move dir elf elves

part1 :: String -> Int
part1 = (\(c, minX, minY, maxX, maxY) -> (maxX - minX + 1)*(maxY - minY + 1) - c)
        . S.foldl' (\(c, minX, minY, maxX, maxY) (V2 x y) ->
                        (c+1, min minX x, min minY y, max maxX x, max maxY y))
              (0, maxBound, maxBound, minBound, minBound) . (!! 10) . steps

part2 :: String -> Maybe Int
part2 input = fmap (+1) $ findIndex id $ zipWith (==) sts $ tail sts
    where sts = steps input
