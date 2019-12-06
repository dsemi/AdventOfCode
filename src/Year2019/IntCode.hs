module Year2019.IntCode where

import Control.Lens
import Data.Char
import Data.List.Split (splitOn)
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V


data Program = Program { _pointer :: Int
                       , _input :: [Int]
                       , _output :: [Int]
                       , _instrs :: Vector Int
                       }
makeLenses ''Program

parse :: String -> Program
parse = Program 0 [] [] . V.fromList . map read . splitOn ","

revDigits :: Int -> [Int]
revDigits = (++ repeat 0) . map digitToInt . reverse . show

evalInstr :: Program -> Program
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
                   1 -> (pointer +~ 4)
                        . (addr 3 .~ val 1 + val 2)
                   -- Multiplication
                   2 -> (pointer +~ 4)
                        . (addr 3 .~ val 1 * val 2)
                   -- Store
                   3 -> (pointer +~ 2)
                        . (input %~ tail)
                        . (addr 1 .~ prog ^?! input . ix 0)
                   -- Print
                   4 -> (pointer +~ 2)
                        . (output %~ (++ [val 1]))
                   -- Jump if true
                   5 -> (pointer .~ (if val 1 /= 0 then val 2 else i + 3))
                   -- Jump if false
                   6 -> (pointer .~ (if val 1 == 0 then val 2 else i + 3))
                   -- Less than
                   7 -> (pointer +~ 4)
                        . (addr 3 .~ (if val 1 < val 2 then 1 else 0))
                   -- Equal
                   8 -> (pointer +~ 4)
                        . (addr 3 .~ (if val 1 == val 2 then 1 else 0))
                   x -> error $ "Unknown instruction: " ++ show x

runInternal :: Program -> Program
runInternal prog
    -- Finish
    | prog ^?! instrs . ix (prog ^. pointer) == 99 = prog
    | otherwise = runInternal $ evalInstr prog

runV1 :: Int -> Int -> Program -> Int
runV1 a b = (^?! instrs . ix 0) . runInternal . (instrs . ix 2 .~ b) . (instrs . ix 1 .~ a)

runV2 :: Int -> Program -> Int
runV2 inp = getOutput . (^. output) . runInternal . (input %~ (inp :))
    where getOutput [x] = x
          getOutput [] = error "Empty output"
          getOutput (x:xs)
              | x /= 0 = error $ "Failed with: " ++ show x
              | otherwise = getOutput xs
