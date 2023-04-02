module Year2016.Day21
    ( part1
    , part2
    ) where

import Control.Applicative (asum)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.List (foldl')
import Data.Maybe (fromJust)
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Mutable as MV
import FlatParse.Basic hiding (take)

data Dir = L | R

data Action = SwapPos Int Int
            | SwapChr Char Char
            | Rotate Dir Int
            | RotateByChr Char
            | RotateByChrInv Char
            | Reverse Int Int
            | Move Int Int

parseActions :: ByteString -> [Action]
parseActions = map (\line -> case runParser (asum actions) line of
                               OK res _ -> res
                               _ -> error "unreachable") . B.lines

actions :: [Parser () Action]
actions = [ SwapPos <$> ($(string "swap position ") *> anyAsciiDecimalInt) <*>
                        ($(string " with position ") *> anyAsciiDecimalInt)
          , SwapChr <$> ($(string "swap letter ") *> anyAsciiChar) <*>
                        ($(string " with letter ") *> anyAsciiChar)
          , Rotate R <$> ($(string "rotate right ") *> anyAsciiDecimalInt <*
                         $(string " step") <* optional_ $(char 's'))
          , Rotate L <$> ($(string "rotate left ") *> anyAsciiDecimalInt <*
                         $(string " step") <* optional_ $(char 's'))
          , RotateByChr <$> ($(string "rotate based on position of letter ") *> anyAsciiChar)
          , Reverse <$> ($(string "reverse positions ") *> anyAsciiDecimalInt) <*>
                        ($(string " through ") *> anyAsciiDecimalInt)
          , Move <$> ($(string "move position ") *> anyAsciiDecimalInt) <*>
                     ($(string " to position ") *> anyAsciiDecimalInt) ]

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


part1 :: ByteString -> String
part1 = eval "abcdefgh" . parseActions

invert :: Action -> Action
invert (Rotate L n) = Rotate R n
invert (Rotate R n) = Rotate L n
invert (RotateByChr c) = RotateByChrInv c
invert (Move a b) = Move b a
invert x = x

part2 :: ByteString -> String
part2 = eval "fbgdceah" . map invert . reverse . parseActions
