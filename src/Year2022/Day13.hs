{-# LANGUAGE OverloadedStrings #-}

module Year2022.Day13
    ( part1
    , part2
    ) where

import Data.List (findIndices, sort)
import Data.List.Split (splitOn)
import Data.Maybe
import Text.Megaparsec
import Text.Megaparsec.Char.Lexer

data Packet = Lit Int | List [Packet] deriving (Eq)

instance Ord Packet where
    compare (Lit a) (Lit b) = compare a b
    compare a@(Lit _) b@(List _) = compare (List [a]) b
    compare a@(List _) b@(Lit _) = compare a $ List [b]
    compare (List as) (List bs) = foldr (<>) (compare (length as) (length bs)) (zipWith compare as bs)

readPacket :: String -> Packet
readPacket = fromJust . parseMaybe @() packet
    where packet = (Lit <$> decimal) <|> (List <$> between "[" "]" (packet `sepBy` ","))

part1 :: String -> Int
part1 = sum . map check . zip [1..] . splitOn "\n\n"
    where check (i, pkts) = case map readPacket (lines pkts) of
                              [p1, p2] -> if p1 < p2 then i else 0
                              _ -> error "Malformed input"

part2 :: String -> Int
part2 input = product $ map (+1) $ findIndices (`elem` dividers) packets
    where dividers = [readPacket "[[2]]", readPacket "[[6]]"]
          packets = sort $ (dividers ++) $ concatMap (map readPacket . lines) $ splitOn "\n\n" input
