module Year2015.Day15
    ( part1
    , part2
    ) where

import Year2015.Utils

import Control.Applicative
import Data.List (transpose)

data Ingredient = Ingredient { capacity :: Int
                             , durability :: Int
                             , flavor :: Int
                             , texture :: Int
                             , calories :: Int
                             }

parseIngredient :: String -> Ingredient
parseIngredient s = let (Right [c, d, f, t, ca]) = findAllInts s
                    in Ingredient c d f t ca

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

part1 :: String -> String
part1 = show . maximum . scores 100 (const True). map parseIngredient . lines

part2 :: String -> String
part2 = show . maximum . scores 100 ((==500) . sum). map parseIngredient . lines
