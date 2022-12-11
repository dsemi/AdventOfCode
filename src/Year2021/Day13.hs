module Year2021.Day13
    ( part1
    , part2
    ) where

import Data.Bool
import Data.HashSet (HashSet)
import qualified Data.HashSet as S
import Data.List (foldl')
import Data.List.Split (splitOn)

import Ocr

parse :: String -> (HashSet (Int, Int), [String])
parse input = let [dots, instrs] = splitOn "\n\n" input
              in ( S.fromList
                   $ map (\dot -> let [x, y] = splitOn "," dot
                                  in (read x, read y))
                         $ lines dots
                 , lines instrs )

fold :: HashSet (Int, Int) -> String -> HashSet (Int, Int)
fold paper instr = let [d, n'] = splitOn "=" $ last $ words instr
                       n = read n'
                   in if d == "x"
                      then S.map (\(x, y) -> (min x (2 * n - x), y)) paper
                      else S.map (\(x, y) -> (x, min y (2 * n - y))) paper

part1 :: String -> Int
part1 input = S.size $ fold paper $ head instrs
    where (paper, instrs) = parse input

part2 :: String -> String
part2 input = parseLetters $ unlines [[bool ' ' '#' $ S.member (x, y) paper | x <- [0..mx]] | y <- [0..my]]
    where (paper', instrs) = parse input
          paper = foldl' fold paper' instrs
          (mx, my) = foldr (\(x, y) (x', y') -> (max x x', max y y')) (0, 0) paper
