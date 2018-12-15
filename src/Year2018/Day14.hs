module Year2018.Day14
    ( part1
    , part2
    ) where

import Data.Char
import Data.List (isPrefixOf, tails)
import Data.Sequence ((><))
import qualified Data.Sequence as S


digits :: Int -> [Int]
digits n
    | n < 10 = [n]
    | otherwise = [1, n `rem` 10]

recipes :: String
recipes = '3' : '7' : go 0 1 (S.fromList [3, 7])
    where go elf1 elf2 rs =
              map intToDigit newRs
                      ++ go (succ (score elf1 + elf1) `rem` S.length rs')
                             (succ (score elf2 + elf2) `rem` S.length rs')
                             rs'
              where score = S.index rs
                    newRs = digits $ score elf1 + score elf2
                    rs' = rs >< S.fromList newRs

part1 :: Int -> String
part1 n = take 10 $ drop n $ recipes

part2 :: String -> Int
part2 n = length $ takeWhile (not . (n `isPrefixOf`)) $ tails recipes
