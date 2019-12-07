{-# LANGUAGE FlexibleContexts #-}

module Year2019.IntCode where

import Control.Lens
import Control.Monad.Writer
import Data.Char
import Data.List.Split (splitOn)
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V


data Program = Program { _pointer :: Int
                       , _input :: [Int]
                       , _instrs :: Vector Int
                       }
makeLenses ''Program

parse :: String -> Program
parse = Program 0 [] . V.fromList . map read . splitOn ","

revDigits :: Int -> [Int]
revDigits = (++ repeat 0) . map digitToInt . reverse . show

data EvalResult = Halt | Output (Int, Program) | Result Program

evalInstr :: Program -> EvalResult
evalInstr prog = eval prog
    where i = prog ^. pointer
          addr a = if modes !! (a - 1) == 1
                   then instrs . ix (i + a) -- Immediate
                   else instrs . ix (prog ^?! instrs . ix (i + a)) -- Position
          val a = prog ^?! addr a
          instr = prog ^?! instrs . ix i `mod` 100
          modes = revDigits $ prog ^?! instrs . ix i `div` 100
          eval = case instr of
                   -- Addition
                   1  -> Result . (pointer +~ 4)
                         . (addr 3 .~ val 1 + val 2)
                   -- Multiplication
                   2  -> Result . (pointer +~ 4)
                         . (addr 3 .~ val 1 * val 2)
                   -- Store
                   3  -> Result . (pointer +~ 2)
                         . (input %~ tail)
                         . (addr 1 .~ prog ^?! input . ix 0)
                   -- Print
                   4  -> Output . (val 1,) . (pointer +~ 2)
                   -- Jump if true
                   5  -> Result . (pointer .~ (if val 1 /= 0 then val 2 else i + 3))
                   -- Jump if false
                   6  -> Result . (pointer .~ (if val 1 == 0 then val 2 else i + 3))
                   -- Less than
                   7  -> Result . (pointer +~ 4)
                         . (addr 3 .~ (if val 1 < val 2 then 1 else 0))
                   -- Equal
                   8  -> Result . (pointer +~ 4)
                         . (addr 3 .~ (if val 1 == val 2 then 1 else 0))
                   -- Stop execution
                   99 -> const Halt
                   x  -> error $ "Unknown instruction: " ++ show x

runInternal :: Program -> (Program, [Int])
runInternal = runWriter . go
    where go prog = case evalInstr prog of
                      Halt -> pure prog
                      Result prog' -> go prog'
                      Output (i, prog') -> tell [i] >> go prog'

runV1 :: Int -> Int -> Program -> Int
runV1 a b = (^?! instrs . ix 0) . fst . runInternal . (instrs . ix 2 .~ b) . (instrs . ix 1 .~ a)

run :: Program -> [Int]
run = snd . runInternal
