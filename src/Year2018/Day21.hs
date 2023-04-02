{-# LANGUAGE FlexibleContexts #-}

module Year2018.Day21
    ( Year2018.Day21.part1
    , Year2018.Day21.part2
    ) where

import Control.Monad.Writer.Lazy
import Data.ByteString (ByteString)
import Data.Either
import qualified Data.IntSet as S
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Vector.Unboxed ((!), (//))
import qualified Data.Vector.Unboxed as U

import Year2018.Day19

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

run :: U.Vector Int -> Int -> Vector Instr -> [Int]
run registers ip instrs = go 0 registers
    where go i regs
              | i < 0 || i >= V.length instrs = []
              | otherwise =
                  let (v, regs') = case (innerLoop i ip $ V.toList $ V.drop i instrs) of
                                     Just f -> (Nothing, f regs)
                                     Nothing -> eval True (regs // [(ip, i)]) (instrs V.! i)
                  in case v of
                       Just n -> n : go (regs' ! ip + 1) regs'
                       Nothing -> go (regs' ! ip + 1) regs'

part1 :: ByteString -> Int
part1 = head . uncurry (run $ U.replicate 6 0) . parseInstrs

lastUnique :: [Int] -> Int
lastUnique = fromLeft undefined . foldM go (S.empty, 0)
    where go (s, y) x
              | S.member x s = Left y
              | otherwise = Right (S.insert x s, x)

part2 :: ByteString -> Int
part2 = lastUnique . uncurry (run $ U.replicate 6 0) . parseInstrs
