{-# LANGUAGE FlexibleContexts, TypeApplications, ViewPatterns #-}

module Year2018.Day21
    ( part1
    , part2
    ) where

import Control.Monad.Writer.Lazy
import Data.Bits
import Data.Bool
import qualified Data.IntSet as S
import Data.Maybe
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Vector.Unboxed ((!), (//))
import qualified Data.Vector.Unboxed as U
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer


data Op = Addr | Addi | Mulr | Muli | Banr | Bani | Borr | Bori
        | Setr | Seti | Gtir | Gtri | Gtrr | Eqir | Eqri | Eqrr
          deriving (Show)

data Instr = Instr Op Int Int Int deriving (Show)

parseInstrs :: String -> (Int, Vector Instr)
parseInstrs = fromJust . parseMaybe @() (do
                ip <- string "#ip " *> decimal <* newline
                instrs <- V.fromList <$> instr `sepBy` newline
                pure (ip, instrs))
    where instr = do
            op <- choice [ string "addr" *> pure Addr
                         , string "addi" *> pure Addi
                         , string "mulr" *> pure Mulr
                         , string "muli" *> pure Muli
                         , string "banr" *> pure Banr
                         , string "bani" *> pure Bani
                         , string "borr" *> pure Borr
                         , string "bori" *> pure Bori
                         , string "setr" *> pure Setr
                         , string "seti" *> pure Seti
                         , string "gtir" *> pure Gtir
                         , string "gtri" *> pure Gtri
                         , string "gtrr" *> pure Gtrr
                         , string "eqir" *> pure Eqir
                         , string "eqri" *> pure Eqri
                         , string "eqrr" *> pure Eqrr ]
            [a, b, c] <- char ' ' *> (decimal `sepBy` char ' ')
            pure $ Instr op a b c

innerLoop :: Int -> Int -> [Instr] -> Maybe (U.Vector Int -> U.Vector Int)
innerLoop i ip ( (Instr Seti 0 _ t)
               : (Instr Addi ((==t) -> True) 1 u)
               : (Instr Muli ((==u) -> True) n ((==u) -> True))
               : (Instr Gtrr ((==u) -> True) r ((==u) -> True))
               : (Instr Addr ((==u) -> True) ((==ip) -> True) ((==ip) -> True))
               : (Instr Addi ((==ip) -> True) 1 ((==ip) -> True))
               : (Instr Seti ((==i+8) -> True) _ ((==ip) -> True))
               : (Instr Addi ((==t) -> True) ((==u) -> True) ((==t) -> True))
               : (Instr Seti ((==i) -> True) _ ((==ip) -> True))
               : _) = Just $ \v -> v // [(ip, i+8), (t, max 0 $ v ! r `div` n), (u, 1)]
innerLoop _ _ _ = Nothing

eval :: (MonadWriter [Int] m) => U.Vector Int -> Instr -> m (U.Vector Int)
eval v (Instr op a b c) =
    case op of
      Addr -> pure $ v // [(c, v ! a + v ! b)]
      Addi -> pure $ v // [(c, v ! a + b)]
      Mulr -> pure $ v // [(c, v ! a * v ! b)]
      Muli -> pure $ v // [(c, v ! a * b)]
      Banr -> pure $ v // [(c, v ! a .&. v ! b)]
      Bani -> pure $ v // [(c, v ! a .&. b)]
      Borr -> pure $ v // [(c, v ! a .|. v ! b)]
      Bori -> pure $ v // [(c, v ! a .|. b)]
      Setr -> pure $ v // [(c, v ! a)]
      Seti -> pure $ v // [(c, a)]
      Gtir -> pure $ v // [(c, bool 0 1 $ a > v ! b)]
      Gtri -> pure $ v // [(c, bool 0 1 $ v ! a > b)]
      Gtrr -> pure $ v // [(c, bool 0 1 $ v ! a > v ! b)]
      Eqir -> pure $ v // [(c, bool 0 1 $ a == v ! b)]
      Eqri -> pure $ v // [(c, bool 0 1 $ v ! a == b)]
      Eqrr -> do
        tell [v ! a]
        pure $ v // [(c, bool 0 1 $ v ! a == v ! b)]

runProg :: U.Vector Int -> Int -> Vector Instr -> [Int]
runProg registers ip instrs = execWriter (go 0 registers)
    where go i regs
              | i < 0 || i >= V.length instrs = pure ()
              | otherwise = do
                regs' <- case (innerLoop i ip $ V.toList $ V.drop i instrs) of
                           Just f -> pure $ f regs
                           Nothing -> eval (regs // [(ip, i)]) (instrs V.! i)
                go (regs' ! ip + 1) regs'

part1 :: String -> Int
part1 = head . uncurry (runProg $ U.replicate 6 0) . parseInstrs

findCycle :: [Int] -> Int
findCycle = go S.empty 0
    where go s y (x:xs)
              | S.member x s = y
              | otherwise = go (S.insert x s) x xs

part2 :: String -> Int
part2 = findCycle . uncurry (runProg $ U.replicate 6 0) . parseInstrs
