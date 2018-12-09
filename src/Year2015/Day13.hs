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
    where addEdgeToMap (Edge p1 p2 n) m = let m' = fromMaybe M.empty $ M.lookup p1 m
                                          in M.insert p1 (M.insert p2 n m') m

maxHappinessOrdering :: HashMap Char (HashMap Char Int) -> Int
maxHappinessOrdering m = maximum $ map happinessDiff orders
    where orders = permutations $ M.keys m
          happinessDiff [] = error "No vals"
          happinessDiff xss@(x:_) = go 0 xss
              where go _ [] = error "No vals"
                    go !c [y] = c + hd x y
                    go !c (a:b:xs) = go (c + hd a b) (b:xs)
          hd a b = m ! a ! b + m ! b ! a

part1 :: String -> Int
part1 = maxHappinessOrdering . constructMap . map parseLine . lines

part2 :: String -> Int
part2 input = let m = constructMap . map parseLine $ lines input
                  meMap = M.fromList . zip (M.keys m) $ repeat 0
                  m' = M.insert 'X' meMap $ M.map (M.insert 'X' 0) m
              in maxHappinessOrdering m'
