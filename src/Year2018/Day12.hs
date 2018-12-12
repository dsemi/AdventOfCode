module Year2018.Day12
    ( part1
    , part2
    ) where

import Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as M


type Map = HashMap (Char, Char, Char, Char, Char) Char

parse :: String -> (Map, [(Int, Char)])
parse s = let (initial:_:mappings) = lines s
          in (M.fromList $ map f mappings, zip [0..] $ drop 15 initial)
    where f (a:b:c:d:e:rest) = ((a, b, c, d, e), last rest)

nextGen :: Map -> [(Int, Char)] -> [(Int, Char)]
nextGen m s = go $ [(x0-3, '.'), (x0-2, '.'), (x0-1, '.')] ++ s
              ++ [(x1+1, '.'), (x1+2, '.'), (x1+3, '.')]
    where go ((_,a):(bi,b):(ci,c):(di,d):(ei,e):rest) = (ci, m ! (a, b, c, d, e))
                                                        : go ((bi,b):(ci,c):(di,d):(ei,e):rest)
          go _ = []
          (x0, x1) = (fst (head s), fst (last s))

sumIndices :: [(Int, Char)] -> Int
sumIndices = sum . map (\(i, v) -> if v == '#' then i else 0)

findArith :: Map -> [(Int, Char)] -> (Int, Int, Int)
findArith m start = go 0 0 start
    where go c prev x
              | prev' == prev = (c, prev, sumIndices x)
              | otherwise = go (c+1) prev' x'
              where x' = nextGen m x
                    prev' = sumIndices x' - sumIndices x


part1 :: String -> Int
part1 input = let (m, start) = parse input
              in sumIndices $ iterate (nextGen m) start !! 20

part2 :: String -> Int
part2 input = let (n, diff, tot) = uncurry findArith $ parse input
              in (50000000000 - n) * diff + tot
