{-# LANGUAGE FlexibleContexts, NoFieldSelectors, OverloadedRecordDot #-}

module Year2017.Day18
    ( part1
    , part2
    ) where

import Control.Applicative (asum)
import Data.Array.Unboxed
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Vector (Vector)
import qualified Data.Vector as V
import FlatParse.Basic

import Utils

type Value = Either Char Int
data Instr = Snd Value
           | Set Char Value
           | Add Char Value
           | Mul Char Value
           | Mod Char Value
           | Rcv Char
           | Jgz Value Value

data Sim = Sim { regs :: UArray Char Int
               , line :: Int
               , instrs :: Vector Instr
               }

parseInstrs :: ByteString -> Sim
parseInstrs = Sim (listArray ('a', 'z') $ repeat 0) 0 . V.fromList
              . map parse . B.lines
    where parse ln = case runParser instr ln of
                       OK res _ -> res
                       _ -> error "unreachable"
          instr = asum [ $(string "snd ") >> Snd <$> value
                       , $(string "set ") >> Set <$> letterChar <* $(char ' ') <*> value
                       , $(string "add ") >> Add <$> letterChar <* $(char ' ') <*> value
                       , $(string "mul ") >> Mul <$> letterChar <* $(char ' ') <*> value
                       , $(string "mod ") >> Mod <$> letterChar <* $(char ' ') <*> value
                       , $(string "rcv ") >> Rcv <$> letterChar
                       , $(string "jgz ") >> Jgz <$> value <* $(char ' ') <*> value
                       ]
          letterChar = satisfy isLatinLetter
          value = (Left <$> letterChar) <|> (Right <$> signedInt)

set :: Char -> Int -> Sim -> Sim
set r v sim = sim { regs = sim.regs // [(r, v)] }

data Sig = Send Int | Recv Int deriving (Eq)

isRecv :: Sig -> Bool
isRecv (Recv _) = True
isRecv _ = False

couple :: [Sig] -> [Sig]
couple [] = []
couple (x:xs) = if isRecv x then xs else x : couple xs

run :: Sim -> [Sig] -> [Sig]
run sim inps = case sim.instrs V.! sim.line of
                 (Snd v) -> Send (value v) : go (couple inps) id
                 (Set r v) -> go inps $ set r (value v)
                 (Add r v) -> go inps $ set r (get r + value v)
                 (Mul r v) -> go inps $ set r (get r * value v)
                 (Mod r v) -> go inps $ set r (get r `mod` value v)
                 (Rcv r) -> Recv (value $ Left r) : case inps of
                                                      Send v : rest -> go rest $ set r v
                                                      _ -> []
                 (Jgz a b) -> go inps $ if value a > 0 then advLine (value b - 1) else id
    where go ins f = run (advLine 1 $ f sim) ins
          advLine n s = s { line = s.line + n }
          get r = sim.regs ! r
          value = either get id

part1 :: ByteString -> Int
part1 input = go 0 sigs
    where sigs = run (parseInstrs input) sigs
          go _ (Send x : xs) = go x xs
          go v (Recv x : xs)
              | x /= 0 = v
              | otherwise = go v xs
          go _ [] = error "no solution"

part2 :: ByteString -> Int
part2 input = length $ filter (not . isRecv) p1
    where sim0 = parseInstrs input
          sim1 = set 'p' 1 sim0
          p0 = run sim0 p1
          p1 = run sim1 p0
