module Year2016.Day21
    ( part1
    , part2
    ) where

import Data.List (foldl')
import Data.Maybe (fromJust)
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Mutable as MV
import Text.Megaparsec
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer (decimal)


data Dir = L | R

data Action = SwapPos Int Int
            | SwapChr Char Char
            | Rotate Dir Int
            | RotateByChr Char
            | RotateByChrInv Char
            | Reverse Int Int
            | Move Int Int

parseActions :: String -> [Action]
parseActions = map (fromJust . parseMaybe (choice actions)) . lines

actions :: [Parsec () String Action]
actions = [ SwapPos <$> (chunk "swap position " *> decimal <* chunk " with position ") <*> decimal
          , SwapChr <$> (chunk "swap letter " *> anySingle) <*> (chunk " with letter " *> anySingle)
          , Rotate R <$> (chunk "rotate right " *> decimal <* chunk " step" <* optional (char 's'))
          , Rotate L <$> (chunk "rotate left " *> decimal <* chunk " step" <* optional (char 's'))
          , RotateByChr <$> (chunk "rotate based on position of letter " *> anySingle)
          , Reverse <$> (chunk "reverse positions " *> decimal) <*> (chunk " through " *> decimal)
          , Move <$> (chunk "move position " *> decimal) <*> (chunk " to position " *> decimal) ]

elemIndex :: Char -> Vector Char -> Int
elemIndex a = fromJust . V.elemIndex a

eval :: String -> [Action] -> String
eval input as = V.toList $ foldl' go (V.fromList input) as
    where go :: Vector Char -> Action -> Vector Char
          go v = \case
                 SwapPos i j -> V.modify (\m -> MV.swap m i j) v
                 SwapChr a b -> go v (SwapPos (elemIndex a v) (elemIndex b v))
                 Rotate L n ->
                     V.backpermute v $ V.fromList $ take (V.length v) $ map (`mod` V.length v) [n..]
                 Rotate R n -> go v (Rotate L (V.length v - n))
                 RotateByChr c -> go v $ Rotate R $ rotateByCharIndex $ elemIndex c v
                 RotateByChrInv c -> snd $ head
                                     $ filter (\(n, m) -> rotateByCharIndex (elemIndex c m) == n)
                                     $ zip [0..] $ iterate (`go` Rotate L 1) v
                 Reverse i j -> V.modify (VGM.reverse . MV.slice i (j - i + 1)) v
                 Move i j -> V.modify (\m -> mapM_ (MV.swap m i) [j, if i<j then j-1 else j+1..i]) v
          rotateByCharIndex i = if i >= 4 then i + 2 else i + 1


part1 :: String -> String
part1 = eval "abcdefgh" . parseActions

invert :: Action -> Action
invert (Rotate L n) = Rotate R n
invert (Rotate R n) = Rotate L n
invert (RotateByChr c) = RotateByChrInv c
invert (Move a b) = Move b a
invert x = x

part2 :: String -> String
part2 = eval "fbgdceah" . map invert . reverse . parseActions
