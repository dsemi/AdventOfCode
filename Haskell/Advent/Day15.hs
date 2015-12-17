{-# LANGUAGE QuasiQuotes #-}

module Advent.Day15
    ( part1
    , part2
    ) where

import Advent.Problem

import Control.Applicative
import Data.List (transpose)
import Text.Regex.PCRE.Heavy (re, scan)

data Ingredient = Ingredient { capacity :: Int
                             , durability :: Int
                             , flavor :: Int
                             , texture :: Int
                             , calories :: Int
                             }

parseIngredient :: String -> Ingredient
parseIngredient s = let [c, d, f, t, ca] = map (read . fst) $ scan regex s
                    in Ingredient c d f t ca
    where regex = [re|-?\d+|]

partitions :: Int -> Int -> [[Int]]
partitions 1 t = [[t]]
partitions n t = [ x : xs | x <- [0..t], xs <- partitions (n-1) $ t-x ]

scores :: Int -> ([Int] -> Bool) -> [Ingredient] -> [Int]
scores total calFilter ings =
    [ product . map (max 0 . sum) $ transpose totes
    | ms <- partitions (length ings) total
    , let totes = zipWith (\n i -> map (n*) (scorings <*> pure i)) ms ings
    , calFilter $ zipWith (\n i -> n * calories i) ms ings
    ]
    where scorings = [capacity, durability, flavor, texture]

part1 :: Problem
part1 = Pure $ maximum . scores 100 (const True). map parseIngredient . lines

part2 :: Problem
part2 = Pure $ maximum . scores 100 ((==500) . sum). map parseIngredient . lines
