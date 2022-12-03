module Year2022.Day03
    ( part1
    , part2
    ) where

import Data.List (elemIndex)
import Data.List.Split
import Data.Maybe
import qualified Data.HashSet as S

alpha :: String
alpha = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

part1 :: String -> Int
part1 = sum . map go . lines
    where go line = let (a, b) = splitAt (length line `div` 2) line
                        x = head $ S.toList $ S.intersection (S.fromList a) (S.fromList b)
                    in fromJust (x `elemIndex` alpha) + 1

part2 :: String -> Int
part2 = sum . map go . chunksOf 3 . lines
    where go [a, b, c] = let x = head $ S.toList
                                 $ foldr1 S.intersection [S.fromList a, S.fromList b, S.fromList c]
                    in fromJust (x `elemIndex` alpha) + 1
          go _ = error "Malformed input"
