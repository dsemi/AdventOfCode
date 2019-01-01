{-# LANGUAGE TypeApplications #-}

module Year2018.Day23
    ( part1
    , part2
    ) where

import Control.Lens
import Data.Ix
import Data.Maybe
import Data.List (maximumBy)
import Data.Ord
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer


type Coord = (Int, Int, Int)

data Nanobot = Nanobot { pos :: Coord
                       , radius :: Int
                       } deriving (Show)

parseNanobots :: String -> [Nanobot]
parseNanobots = map (fromJust . parseMaybe @() nanobot) . lines
    where nanobot = do
            [x, y, z] <- between (string "pos=<") (string ">")
                         $ signed (pure ()) decimal `sepBy` char ','
            r <- string ", r=" *> decimal
            pure $ Nanobot (x, y, z) r

nanoInRange :: Nanobot -> Coord -> Bool
nanoInRange (Nanobot (x, y, z) r) (x', y', z') =
    abs (x - x') + abs (y - y') + abs (z - z') <= r

part1 :: String -> Int
part1 input = let ns = parseNanobots input
                  maxBot = maximumBy (comparing radius) ns
              in length $ filter (nanoInRange maxBot . pos) ns

scaleDown :: Int -> Nanobot -> Nanobot
scaleDown n (Nanobot (x, y, z) r) = Nanobot (x `div` n, y `div` n, z `div` n) (r `div` n)

scale :: Int
scale = 10000000

solve :: [Nanobot] -> Coord
solve ns = go scale (minCoord, maxCoord)
    where minCoord = over each ((`div` scale) . minimum) $ unzip3 $ map pos ns
          maxCoord = over each ((`div` scale) . maximum) $ unzip3 $ map pos ns
          go n (a, b)
              | n == 1 = coord
              | otherwise = go (n `div` 10) ( over each ((*10) . pred) coord
                                            , over each ((*10) . succ) coord )
              where ns' = map (scaleDown n) ns
                    coord = snd $ maximumBy (comparing (\(a', b') -> (a', over each negate b')))
                            $ map (\p -> (length (filter (`nanoInRange` p) ns'), p))
                            $ range (a, b)

part2 :: String -> Int
part2 = (\(a, b, c) -> a + b + c) . solve . parseNanobots
