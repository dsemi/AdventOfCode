{-# LANGUAGE TypeApplications #-}

module Year2018.Day19
    ( part1
    , part2
    ) where

import Data.Bits
import Data.Bool
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
            [a, b, c] <- spaceChar *> (decimal `sepBy` char ' ')
            pure $ Instr op a b c

eval :: U.Vector Int -> Instr -> U.Vector Int
eval v (Instr op a b c) =
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

runProg :: U.Vector Int -> Int -> Vector Instr -> U.Vector Int
runProg registers ip instrs = go 0 registers
    where go i regs
              | i < 0 || i > V.length instrs = regs
              | otherwise = let regs' = eval (regs // [(ip, i)]) (instrs V.! i)
                            in go (regs' ! ip + 1) regs'

part1 :: String -> Int
part1 = (! 0) . uncurry (runProg $ U.replicate 6 0) . parseInstrs

-- Not sure if there's a better way than just deconstructing the assembly
part2 :: String -> Int
part2 _ = sum $ filter ((==0) . (10551361 `rem`)) [1..10551361]
