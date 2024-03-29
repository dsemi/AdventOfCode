module Year2015.Day15
    ( part1
    , part2
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Linear.Vector

import Utils

data Ingredient = Ingredient { capacity :: Int
                             , durability :: Int
                             , flavor :: Int
                             , texture :: Int
                             , calories :: Int
                             }

parseIngredient :: ByteString -> Ingredient
parseIngredient s = let [c, d, f, t, ca] = findAllInts s
                    in Ingredient c d f t ca

partitions :: Int -> Int -> [[Int]]
partitions 1 t = [[t]]
partitions n t = [ x : xs | x <- [0..t], xs <- partitions (n-1) $ t-x ]

scores :: Int -> (Int -> Bool) -> ByteString -> [Int]
scores total calFilter input =
    [ product . map (max 0) $ sumV totes
    | ms <- partitions (length ings) total
    , let totes = zipWith (\n i -> map (n*) (scorings <*> pure i)) ms ings
    , calFilter $ sum $ zipWith (\n i -> n * calories i) ms ings
    ]
    where ings = map parseIngredient $ B.lines input
          scorings = [capacity, durability, flavor, texture]

part1 :: ByteString -> Int
part1 = maximum . scores 100 (const True)

part2 :: ByteString -> Int
part2 = maximum . scores 100 (==500)
