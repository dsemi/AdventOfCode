module Year2018.Day16
    ( part1
    , part2
    ) where

import Data.Bits
import Data.Bool
import Data.Set (Set)
import qualified Data.Set as S
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as M
import Data.List (foldl')
import Data.List.Split (splitOn)
import Data.Maybe
import Data.Vector (Vector, (!), (//))
import qualified Data.Vector as V
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer


testSample :: String -> (Int, Set Op)
testSample = fromJust . parseMaybe @() (do
                bef <- V.fromList <$> (string "Before: " *> arr)
                [op, a, b, c] <- newline *> (decimal `sepBy` char ' ')
                aft <- V.fromList <$> (newline *> string "After:  " *> arr)
                pure (op, S.fromList $ filter (\cmd -> eval bef cmd a b c == aft) [minBound..]))
    where arr = between (char '[') (char ']') (decimal `sepBy` string ", ")

data Op = Addr | Addi | Mulr | Muli | Banr | Bani | Borr | Bori
        | Setr | Seti | Gtir | Gtri | Gtrr | Eqir | Eqri | Eqrr
          deriving (Bounded, Enum, Eq, Ord)

eval :: Vector Int -> Op -> Int -> Int -> Int -> Vector Int
eval v op a b c = v // [(c, v')]
    where v' = case op of
                 Addr -> v ! a + v ! b
                 Addi -> v ! a + b
                 Mulr -> v ! a * v ! b
                 Muli -> v ! a * b
                 Banr -> v ! a .&. v ! b
                 Bani -> v ! a .&. b
                 Borr -> v ! a .|. v ! b
                 Bori -> v ! a .|. b
                 Setr -> v ! a
                 Seti -> a
                 Gtir -> bool 0 1 $ a > v ! b
                 Gtri -> bool 0 1 $ v ! a > b
                 Gtrr -> bool 0 1 $ v ! a > v ! b
                 Eqir -> bool 0 1 $ a == v ! b
                 Eqri -> bool 0 1 $ v ! a == b
                 Eqrr -> bool 0 1 $ v ! a == v ! b

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
