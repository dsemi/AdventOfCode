module Year2021.Day14
    ( part1
    , part2
    ) where

import Data.HashMap.Strict ((!))
import qualified Data.HashMap.Strict as M
import Data.List (tails)
import Data.List.Split (splitOn)

polymerize :: Int -> String -> Int
polymerize n input = let vs = lets $ iterate go cnts !! n
                     in maximum vs - minimum vs
    where [tmpl, rest] = splitOn "\n\n" input
          d = M.fromList $ map (\ln -> let [a, (b:_)] = splitOn " -> " ln in (a, b)) $ lines rest
          cnts = M.fromListWith (+) $ takeWhile ((>1) . length . fst)
                 $ map ((,1) . take 2) $ tails $ tmpl
          go = M.fromListWith (+) . concatMap (\(k, v) -> [ ([k !! 0, d ! k], v)
                                                          , ([d ! k, k !! 1], v)]) . M.toList
          lets = M.elems . M.fromListWith (+) . ((last tmpl, 1):)
                 . map (\(k, v) -> (head k, v)) . M.toList

part1 :: String -> Int
part1 = polymerize 10

part2 :: String -> Int
part2 = polymerize 40
