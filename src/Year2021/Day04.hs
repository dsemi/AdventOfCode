module Year2021.Day04
    ( part1
    , part2
    ) where

import Data.IntSet (IntSet)
import qualified Data.IntSet as S
import Data.List (partition, transpose)
import Data.List.Split


isWinner :: IntSet -> [[Int]] -> Bool
isWinner s brd = any (all (`S.member` s)) brd || any (all (`S.member` s)) (transpose brd)

winnerScores :: String -> [Int]
winnerScores input = let boards = map (map (map read . words) . lines) brds
                     in go S.empty boards $ map read $ splitOn "," nums
    where (nums : brds) = splitOn "\n\n" input
          go _ _ [] = []
          go s boards (n:ns) = let s' = S.insert n s
                                   (winners, rest) = partition (isWinner s') boards
                               in map ((*n) . sum . concatMap (filter (`S.notMember` s'))) winners
                                      ++ go s' rest ns

part1 :: String -> Int
part1 = head . winnerScores

part2 :: String -> Int
part2 = last . winnerScores
