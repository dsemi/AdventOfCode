{-# LANGUAGE BangPatterns, OverloadedStrings #-}

module Year2015.Day13
    ( part1
    , part2
    ) where

import Data.Array.Unboxed
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.HashMap.Strict as M
import Data.List (nub)
import FlatParse.Basic

import Utils

parseLine :: ByteString -> ((Char, Char), Int)
parseLine line = case runParser parser line of
                   OK edge _ -> edge
                   _ -> error "unreachable"
    where letter = byteStringOf (some $ satisfy (\x -> isDigit x || isLatinLetter x))
          parseValue = do
            op <- ($(string "lose ") *> pure negate)
                  <|> ($(string "gain ") *> pure id)
            i <- anyAsciiDecimalInt
            pure $ op i
          parser = do
            p1 <- letter <* $(string " would ")
            hap <- parseValue <* $(string " happiness units by sitting next to ")
            p2 <- letter <* $(char '.')
            pure $ ((B.head p1, B.head p2), hap)

adjMap :: [((Char, Char), Int)] -> UArray (Int, Int) Int
adjMap xs = let idx = M.fromList $ zip (nub $ map (fst . fst) xs) [0..]
                len = maximum $ M.elems idx
            in accumArray (+) 0 ((0, 0), (len, len)) $
               concat [[((ka, kb), v), ((kb, ka), v)] | ((a, b), v) <- xs
                      , let ka = idx M.! a
                            kb = idx M.! b ]

part1 :: ByteString -> Int
part1 = heldKarp max . adjMap . map parseLine . B.lines

part2 :: ByteString -> Int
part2 input = let adj = adjMap $ map parseLine $ B.lines input
                  a = fst $ snd $ bounds adj
              in heldKarp max $ array ((0, 0), (a+1, a+1)) $ assocs adj
