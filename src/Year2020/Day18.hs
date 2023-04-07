module Year2020.Day18
    ( part1
    , part2
    ) where

import Data.Char
import Data.Maybe
import Text.ParserCombinators.ReadP

add :: ReadP (Int -> Int -> Int)
add = (+) <$ (string " + ")

mul :: ReadP (Int -> Int -> Int)
mul = (*) <$ (string " * ")

parseExprs :: ReadP (Int -> Int -> Int) -> ReadP (Int -> Int -> Int) -> String -> Maybe [Int]
parseExprs a b = traverse parse . lines
    where parse line = fmap fst . listToMaybe $ readP_to_S (expr <* eof) line
          int = digitToInt <$> satisfy isDigit
          term ex = int +++ between (char '(') (char ')') ex
          expr = chainl1 (chainl1 (term expr) a) b

part1 :: String -> Maybe Int
part1 = fmap sum . parseExprs (add +++ mul) (pure $ error "should be simplified")

part2 :: String -> Maybe Int
part2 = fmap sum . parseExprs add mul
