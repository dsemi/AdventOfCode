module Year2022.Day03
    ( part1
    , part2
    ) where

import Data.List.Split
import qualified Data.HashSet as S

alpha :: [(Char, Int)]
alpha = zip "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" [1..]

part1 :: String -> Maybe Int
part1 = fmap sum . mapM go . lines
    where go line = let (a, b) = splitAt (length line `div` 2) line
                        x = head $ S.toList $ S.intersection (S.fromList a) (S.fromList b)
                    in lookup x alpha

part2 :: String -> Maybe Int
part2 = fmap sum . mapM go . chunksOf 3 . lines
    where go [a, b, c] = let x = head $ S.toList
                                 $ foldr1 S.intersection [S.fromList a, S.fromList b, S.fromList c]
                         in lookup x alpha
          go _ = error "Malformed input"
