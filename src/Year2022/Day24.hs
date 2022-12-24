{-# LANGUAGE BangPatterns #-}

module Year2022.Day24
    ( part1
    , part2
    ) where

import Control.Monad.State.Strict
import Data.HashSet (HashSet)
import qualified Data.HashSet as S
import Linear.V2

data Valley = Valley Int Int [(V2 Int, V2 Int)] (HashSet (V2 Int))

parse :: String -> (V2 Int, V2 Int, Valley)
parse input = (start, goal, Valley w h blizz walls)
    where w = length (head $ lines input) - 2
          h = length (lines input) - 2
          start = V2 0 1
          goal = V2 (h+1) w
          grid = [ (V2 r c, v) | (r, row) <- zip [0..] $ lines input
                 , (c, v) <- zip [0..] row]
          blizz = [ (p, d) | (p, v) <- grid
                  , v `elem` "^v<>"
                  , let d = case v of
                              '^' -> V2 (-1) 0
                              'v' -> V2 1 0
                              '<' -> V2 0 (-1)
                              '>' -> V2 0 1
                              _ -> error "impossible"]
          walls = S.insert (goal + V2 1 0) $ S.insert (start + V2 (-1) 0)
                  $ S.fromList [ p | (p, v) <- grid, v == '#' ]

shortestPath :: V2 Int -> V2 Int -> Valley -> (Int, Valley)
shortestPath start goal (Valley w h blizzard walls) =
    let (cnt, blizz) = go 0 blizzard (S.singleton start)
    in (cnt, Valley w h blizz walls)
    where go !cnt blizz edges
              | S.member goal edges = (cnt, blizz)
              | otherwise = go (cnt+1) blizz' edges'
              where blizz' = [ (pos', d) | (V2 r c, d@(V2 dr dc)) <- blizz
                             , let pos' = V2 ((r+dr-1) `mod` h + 1) ((c+dc-1) `mod` w + 1) ]
                    blizzSet = S.fromList $ map fst blizz'
                    edges' = S.fromList $
                             [ p | p <- S.toList edges
                             , not (S.member p walls || S.member p blizzSet) ] ++
                             [ p' | p <- S.toList edges
                             , p' <- fmap (+p) [V2 0 (-1), V2 0 1, V2 1 0, V2 (-1) 0]
                             , not (S.member p' walls || S.member p' blizzSet) ]

part1 :: String -> Int
part1 input = fst $ shortestPath start goal valley
    where (start, goal, valley) = parse input

part2 :: String -> Int
part2 input = flip evalState valley $ do
                a <- state (shortestPath start goal)
                b <- state (shortestPath goal start)
                c <- state (shortestPath start goal)
                pure $ a + b + c
    where (start, goal, valley) = parse input
