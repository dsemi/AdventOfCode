{-# LANGUAGE BangPatterns #-}

module Year2015.Day13
    ( part1
    , part2
    ) where

import Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as M
import Data.List (permutations)
import Data.Maybe
import Text.Megaparsec
import Text.Megaparsec.Char (alphaNumChar, char, spaceChar, string)
import Text.Megaparsec.Char.Lexer (decimal)


data Edge = Edge Char Char Int

parseLine :: String -> Edge
parseLine = fromJust . parseMaybe parser
    where int = fromInteger <$> decimal
          parseValue :: Parsec () String Int
          parseValue = do
            op <- (try (string "lose") <|> (string "gain")) <* spaceChar
            i <- int
            let op' = if op == "lose" then negate else id
            return $ op' i
          parser :: Parsec () String Edge
          parser = do
            p1 <- some alphaNumChar <* string " would "
            hap <- parseValue <* string " happiness units by sitting next to "
            p2 <- some alphaNumChar <* char '.'
            return $ Edge (head p1) (head p2) hap


constructMap :: [Edge] -> HashMap Char (HashMap Char Int)
constructMap = foldr addEdgeToMap M.empty
    where addEdgeToMap (Edge p1 p2 n) m = let m' = M.lookupDefault M.empty p1 m
                                              m'' = M.insert p1 (M.insertWith (+) p2 n m') m
                                              m''' = M.lookupDefault M.empty p2 m''
                                          in M.insert p2 (M.insertWith (+) p1 n m''') m''

maxHappinessOrdering :: Bool -> HashMap Char (HashMap Char Int) -> Int
maxHappinessOrdering p1 m = maximum $ map happinessDiff $ permutations $ M.keys m
    where happinessDiff [] = error "No vals"
          happinessDiff xss@(x:_) = go 0 xss
              where go _ [] = error "No vals"
                    go !c [y] = if p1 then c + hd x y else c
                    go !c (a:b:xs) = go (c + hd a b) (b:xs)
          hd a b = m ! a ! b

part1 :: String -> Int
part1 = maxHappinessOrdering True . constructMap . map parseLine . lines

part2 :: String -> Int
part2 = maxHappinessOrdering False . constructMap . map parseLine . lines
