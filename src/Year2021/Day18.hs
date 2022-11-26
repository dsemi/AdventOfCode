{-# LANGUAGE RankNTypes #-}

module Year2021.Day18
    ( part1
    , part2
    ) where

import Control.Lens
import Control.Parallel.Strategies (parBuffer, runEval, rseq)
import Data.List (foldl1')
import Data.Maybe
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

import Utils

data Snailfish = Reg Int | Pair { _left :: Snailfish, _right :: Snailfish }
makeLenses ''Snailfish

snailfish :: String -> [Snailfish]
snailfish = fromJust . traverse (parseMaybe @() go) . lines
    where go = (Reg <$> decimal) <|> (Pair <$> (char '[' *> go <* char ',') <*> (go <* char ']'))

addToLeaf :: ASetter' Snailfish Snailfish -> Int -> Snailfish -> Snailfish
addToLeaf _ 0 x = x
addToLeaf _ n (Reg v) = Reg (v + n)
addToLeaf f n fish = over f (addToLeaf f n) fish

explode :: Snailfish -> Maybe Snailfish
explode = fmap snd . go 0
    where go _ (Reg _) = Nothing
          go 4 (Pair (Reg a) (Reg b)) = Just ((a, b), Reg 0)
          go d (Pair a b) = explodeL <$> go (d+1) a <|> explodeR <$> go (d+1) b
              where explodeL ((x, y), newL) = ((x, 0), Pair newL (addToLeaf left y b))
                    explodeR ((x, y), newR) = ((0, y), Pair (addToLeaf right x a) newR)

split :: Snailfish -> Maybe Snailfish
split (Reg n)
    | n > 9 = Just (Pair (Reg (n `div` 2)) (Reg (n - n `div` 2)))
    | otherwise = Nothing
split (Pair a b) = (flip Pair b) <$> split a <|> Pair a <$> split b

add :: Snailfish -> Snailfish -> Snailfish
add a b = go (Pair a b)
    where go n = maybe n go (explode n <|> split n)

mag :: Snailfish -> Int
mag (Reg n) = n
mag (Pair a b) = 3*mag a + 2*mag b

part1 :: String -> Int
part1 = mag . foldl1' add . snailfish

part2 :: String -> IO Int
part2 input = parallel $ maximum $ runEval $ parBuffer 1000 rseq [mag (add x y) | x <- xs, y <- xs]
    where xs = snailfish input
