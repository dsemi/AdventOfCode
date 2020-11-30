{-# LANGUAGE DeriveGeneric #-}

module Year2018.Day16
    ( part1
    , part2
    ) where

import Data.Bits
import Data.Bool
import Data.Hashable
import Data.HashSet (HashSet)
import qualified Data.HashSet as S
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as M
import Data.List (foldl')
import Data.List.Split (splitOn)
import Data.Maybe
import Data.Vector (Vector, (!), (//))
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer


testSample :: String -> (Int, HashSet Op)
testSample = fromJust . parseMaybe @() (do
                bef <- V.fromList <$> (string "Before: " *> arr)
                [op, a, b, c] <- newline *> (decimal `sepBy` char ' ')
                aft <- V.fromList <$> (newline *> string "After:  " *> arr)
                pure (op, S.fromList $ filter (\cmd -> eval bef cmd a b c == aft) [minBound..]))
    where arr = between (char '[') (char ']') (decimal `sepBy` string ", ")

data Op = Addr | Addi | Mulr | Muli | Banr | Bani | Borr | Bori
        | Setr | Seti | Gtir | Gtri | Gtrr | Eqir | Eqri | Eqrr
          deriving (Bounded, Enum, Eq, Generic)
instance Hashable Op

eval :: Vector Int -> Op -> Int -> Int -> Int -> Vector Int
eval v op a b c =
    case op of
      Addr -> v // [(c, v ! a + v ! b)]
      Addi -> v // [(c, v ! a + b)]
      Mulr -> v // [(c, v ! a * v ! b)]
      Muli -> v // [(c, v ! a * b)]
      Banr -> v // [(c, v ! a .&. v ! b)]
      Bani -> v // [(c, v ! a .&. b)]
      Borr -> v // [(c, v ! a .|. v ! b)]
      Bori -> v // [(c, v ! a .|. b)]
      Setr -> v // [(c, v ! a)]
      Seti -> v // [(c, a)]
      Gtir -> v // [(c, bool 0 1 $ a > v ! b)]
      Gtri -> v // [(c, bool 0 1 $ v ! a > b)]
      Gtrr -> v // [(c, bool 0 1 $ v ! a > v ! b)]
      Eqir -> v // [(c, bool 0 1 $ a == v ! b)]
      Eqri -> v // [(c, bool 0 1 $ v ! a == b)]
      Eqrr -> v // [(c, bool 0 1 $ v ! a == v ! b)]

part1 :: String -> Int
part1 = length . filter ((>=3) . S.size . snd) . map testSample . init . init . splitOn "\n\n"

parseInstr :: IntMap Op -> String -> Vector Int -> Vector Int
parseInstr m = fromJust . parseMaybe @() (do
                 [op, a, b, c] <- decimal `sepBy` char ' '
                 pure $ \v -> eval v (m M.! op) a b c)

part2 :: String -> Int
part2 input = let (prog:_:samples) = reverse $ splitOn "\n\n" input
                  m = determineOpCodes $ M.fromListWith S.union $ map testSample samples
              in foldl' (flip ($)) (V.replicate 4 0) (map (parseInstr m) (lines prog)) ! 0
    where determineOpCodes m
              | all ((==1) . S.size) $ M.elems m = M.map (head . S.toList) m
              | otherwise = determineOpCodes
                            $ foldr (\op -> M.map (\v -> bool (S.delete op v) v $ S.size v == 1)) m
                            $ concatMap S.toList $ filter ((==1) . S.size) $ M.elems m
