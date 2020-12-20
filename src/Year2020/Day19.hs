{-# LANGUAGE OverloadedStrings #-}

module Year2020.Day19
    ( part1
    , part2
    ) where

import Data.IntMap (IntMap, (!))
import qualified Data.IntMap as M
import Data.Maybe
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer


type Rule = Either Char [[Int]]

parseInput :: String -> (IntMap Rule, [String])
parseInput = fromJust . parseMaybe @() input
    where input = (,) <$> (M.fromList <$> endBy rule "\n") <* "\n" <*> (some lowerChar `sepBy` "\n")
          rule = (,) <$> decimal <* ": "
                 <*> eitherP ("\"" *> anySingle <* "\"") ((decimal `sepEndBy` " ") `sepBy` "| ")

solve :: [(Int, Rule)] -> String -> Int
solve extras input = length $ filter (any null . go (rss ! 0)) strs
    where (rules, strs) = parseInput input
          rss = M.fromList extras `M.union` rules
          go _ [] = []
          go (Left c) (x:xs) = if c == x then [xs] else []
          go (Right rs) xs = concatMap (foldl (\ys r -> concatMap (go r) ys) [xs] . map (rss !)) rs

part1 :: String -> Int
part1 = solve []

part2 :: String -> Int
part2 = solve [ (8, Right [[42], [42, 8]])
              , (11, Right [[42, 31], [42, 11, 31]]) ]
