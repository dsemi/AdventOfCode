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

import Scanf
import Utils

parseLine :: ByteString -> ((Char, Char), Int)
parseLine line =
    let (a :+ op :+ hap :+ b :+ ()) = scanf [fmt|%s would %s %d happiness units by sitting next to %s|] line
    in if op == "gain"
       then ((B.head a, B.head b), hap)
       else ((B.head a, B.head b), -hap)

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
