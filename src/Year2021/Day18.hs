module Year2021.Day18
    ( part1
    , part2
    ) where

import Data.Maybe
import Data.List (foldl1')
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

data Snailfish = Reg Int | Pair Snailfish Snailfish

snailfish :: String -> [Snailfish]
snailfish = map (fromJust . parseMaybe @() go) . lines
    where go = (Reg <$> decimal) <|> (Pair <$> (char '[' *> go <* char ',') <*> (go <* char ']'))

addL :: Int -> Snailfish -> Snailfish
addL 0 sf = sf
addL n (Reg v) = Reg (v + n)
addL n (Pair a b) = Pair (addL n a) b

addR :: Int -> Snailfish -> Snailfish
addR 0 sf = sf
addR n (Reg v) = Reg (v + n)
addR n (Pair a b) = Pair a (addR n b)

explode :: Snailfish -> Maybe Snailfish
explode = fmap snd . go 0
    where go _ (Reg _) = Nothing
          go 4 (Pair (Reg a) (Reg b)) = Just ((a, b), Reg 0)
          go d (Pair a b) = explodeL <$> go (d+1) a <|> explodeR <$> go (d+1) b
              where explodeL ((x, y), newL) = ((x, 0), Pair newL (addL y b))
                    explodeR ((x, y), newR) = ((0, y), Pair (addR x a) newR)

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

part2 :: String -> Int
part2 (snailfish -> xs) = maximum [mag (add x y) | x <- xs, y <- xs]
