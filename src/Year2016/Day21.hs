module Year2016.Day21
    ( part1
    , part2
    ) where

import Utils

import Control.Lens (_2, over)
import Data.Foldable (toList)
import Data.List (foldl')
import Data.Maybe (mapMaybe)
import Data.Sequence (Seq, (><))
import qualified Data.Sequence as S
import Text.Megaparsec (optional, parseMaybe, (<|>))
import Text.Megaparsec.Char (anyChar, char, string)
import Text.Megaparsec.Char.Lexer (decimal)


data Dir = L | R deriving (Show)
data Action = SwapPosition Int Int
            | SwapChar Char Char
            | Rotate Dir Int
            | RotateByChar Char
            | RotateByCharInv  Char
            | Reverse Int Int
            | Move Int Int deriving (Show)

parser :: Parser Action
parser = parseSwap <|> parseSwapC <|> parseRotateR <|> parseRotateL
         <|> parseRotateC <|> parseReverse <|> parseMove
    where int = fromInteger <$> decimal
          parseSwap = SwapPosition <$> (string "swap position " *> int <* string " with position ") <*> int
          parseSwapC = SwapChar <$> (string "swap letter " *> anyChar) <*> (string " with letter " *> anyChar)
          parseRotateR = Rotate R <$> (string "rotate right " *> int <* string " step" <* optional (char 's'))
          parseRotateL = Rotate L <$> (string "rotate left " *> int <* string " step" <* optional (char 's'))
          parseRotateC = RotateByChar <$> (string "rotate based on position of letter " *> anyChar)
          parseReverse = Reverse <$> (string "reverse positions " *> int) <*> (string " through " *> int)
          parseMove = Move <$> (string "move position " *> int) <*> (string " to position " *> int)

rotateByCharIndex :: Int -> Int
rotateByCharIndex i = if i >= 4 then i + 2 else i + 1

eval :: Seq Char -> Action -> Seq Char
eval s (SwapPosition a b) = S.update b (s `S.index` a) (S.update a (s `S.index` b) s)
eval s (SwapChar a b) = eval s (SwapPosition a' b')
    where Just a' = S.elemIndexL a s
          Just b' = S.elemIndexL b s
eval s (Rotate L n) = b >< a
    where (a, b) = S.splitAt (n `mod` length s) s
eval s (Rotate R n) = eval s $ Rotate L (length s - n)
eval s (RotateByChar c) = eval s $ Rotate R i
    where Just i = rotateByCharIndex <$> S.elemIndexL c s
eval s (RotateByCharInv c) = go s 0
    where go s' n
              | i == n = s'
              | otherwise = go (eval s' (Rotate L 1)) (n+1)
              where Just i = rotateByCharIndex <$> S.elemIndexL c s'
eval s (Reverse x y) = a >< S.reverse b >< c
    where (a, (b, c)) = over _2 (S.splitAt (y-x+1)) $ S.splitAt x s
eval s (Move a b) = S.insertAt b (s `S.index` a) (S.deleteAt a s)

part1 :: String -> String
part1 = toList . foldl' eval (S.fromList "abcdefgh") . mapMaybe (parseMaybe parser) . lines

invert :: [Action] -> [Action]
invert = map inv . reverse
    where inv (Rotate L n) = Rotate R n
          inv (Rotate R n) = Rotate L n
          inv (RotateByChar c) = RotateByCharInv c
          inv (Move a b) = Move b a
          inv x = x

part2 :: String -> String
part2 = toList . foldl' eval (S.fromList "fbgdceah") . invert . mapMaybe (parseMaybe parser) . lines
