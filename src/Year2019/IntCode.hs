module Year2019.IntCode where

import Control.Lens
import Data.List.Split (splitOn)
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V

data Program = Program { _pointer :: Int
                       , _instrs :: Vector Int
                       }
makeLenses ''Program

parse :: String -> Program
parse = Program 0 . V.fromList . map read . splitOn ","

evalInstr :: Program -> Maybe Program
evalInstr prog = eval prog
    where i = prog ^. pointer
          addr a = instrs . ix (prog ^?! instrs . ix a)
          val a = prog ^?! addr a
          instr = prog ^?! instrs . ix i
          eval = case instr of
                   -- Addition
                   1 -> Just . (pointer +~ 4)
                        . (addr (i + 3) .~ val (i + 1) + val (i + 2))
                   -- Multiplication
                   2 -> Just . (pointer +~ 4)
                        . (addr (i + 3) .~ val (i + 1) * val (i + 2))
                   -- Finish
                   99 -> const Nothing
                   _ -> error "Unknown instruction"

run :: Int -> Int -> Program -> Int
run a b = (^?! instrs . ix 0) . go . (instrs . ix 2 .~ b) . (instrs . ix 1 .~ a)
    where go prog = maybe prog go $ evalInstr prog
