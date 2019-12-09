{-# LANGUAGE FlexibleContexts, TemplateHaskell #-}

module Year2019.IntCode
    ( Memory
    , parse
    , runV1
    , runV2
    ) where

import Control.Monad.Free
import Control.Monad.Free.TH
import Data.Bool
import Data.Char
import Data.List.Split (splitOn)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as M


type Memory = IntMap Int

data Program = Program { idx :: !Int
                       , relBase :: !Int
                       , memory :: !Memory
                       }

parse :: String -> Memory
parse = M.fromList . zip [0..] . map read . splitOn ","

build :: Memory -> Program
build = Program 0 0

data Mode = Pos | Imm | Rel deriving (Enum)

getModes :: Int -> [Mode]
getModes = (++ repeat Pos) . map (toEnum . digitToInt) . reverse . show

data Effect a = Input (Int -> a)
              | Output Int a deriving (Functor)
makeFree ''Effect

(!) :: Program -> Int -> Int
(!) prog k = M.findWithDefault 0 k $ memory prog

run :: Program -> Free Effect Program
run prog =
    case instr of
      -- Addition
      1  -> run $ inc 4 $ set 3 (val 1 + val 2) prog
      -- Multiplication
      2  -> run $ inc 4 $ set 3 (val 1 * val 2) prog
      -- Store
      3  -> input >>= \v -> run $ inc 2 $ set 1 v prog
      -- Print
      4  -> output (val 1) >> run (inc 2 prog)
      -- Jump if true
      5  -> run $ bool (inc 3) (jmp $ val 2) (val 1 /= 0) prog
      -- Jump if false
      6  -> run $ bool (inc 3) (jmp $ val 2) (val 1 == 0) prog
      -- Less than
      7  -> run $ inc 4 $ set 3 (bool 0 1 $ val 1 < val 2) prog
      -- Equal
      8  -> run $ inc 4 $ set 3 (bool 0 1 $ val 1 == val 2) prog
      -- Adjust relative base
      9  -> run $ inc 2 $ incRb (val 1) prog
      -- Stop execution
      99 -> pure prog
      x  -> error $ "Unknown instruction: " ++ show x
    where arg n = prog ! (idx prog + n)
          inc n p = p {idx=idx p + n}
          jmp n p = p {idx=n}
          incRb n p = p {relBase=relBase p + n}
          set a v p = case modes !! (a - 1) of
                        Pos -> p {memory=M.insert (arg a) v (memory p)}
                        Imm -> error "set on immediate"
                        Rel -> p {memory=M.insert (arg a + relBase p) v (memory p)}
          val a = case modes !! (a - 1) of
                    Pos -> prog ! arg a
                    Imm -> arg a
                    Rel -> prog ! (arg a + relBase prog)
          instr = (prog ! idx prog) `mod` 100
          modes = getModes $ (prog ! idx prog) `div` 100

runV1 :: Int -> Int -> Memory -> Int
runV1 a b = (! 0) . iter (error "Not pure") . run . build . M.insert 1 a . M.insert 2 b

runV2 :: [Int] -> Memory -> [Int]
runV2 inp = runEffects inp . run . build
    where runEffects xss (Free rest) =
              case rest of
                Input f | x:xs <- xss -> runEffects xs $ f x
                        | otherwise -> error "Not enough inputs"
                Output v eff -> v : runEffects xss eff
          runEffects _ (Pure _) = []
