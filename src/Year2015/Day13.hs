{-# LANGUAGE BangPatterns, OverloadedStrings #-}

module Year2015.Day13
    ( part1
    , part2
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as M
import Data.List (permutations)
import FlatParse.Basic

data Edge = Edge Char Char Int

parseLine :: ByteString -> Edge
parseLine line = case runParser parser line of
                   OK edge _ -> edge
                   _ -> error "unreachable"
    where letter = byteStringOf (some $ satisfy (\x -> isDigit x || isLatinLetter x))
          parseValue = do
            op <- ($(string "lose ") *> pure negate)
                  <|> ($(string "gain ") *> pure id)
            i <- anyAsciiDecimalInt
            return $ op i
          parser = do
            p1 <- letter <* $(string " would ")
            hap <- parseValue <* $(string " happiness units by sitting next to ")
            p2 <- letter <* $(char '.')
            return $ Edge (B.head p1) (B.head p2) hap


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

part1 :: ByteString -> Int
part1 = maxHappinessOrdering True . constructMap . map parseLine . B.lines

part2 :: ByteString -> Int
part2 = maxHappinessOrdering False . constructMap . map parseLine . B.lines
