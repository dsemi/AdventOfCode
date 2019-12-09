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
import Data.IntMap.Strict (IntMap, (!))
import qualified Data.IntMap.Strict as M


type Memory = IntMap Int

parse :: String -> Memory
parse = M.fromList . zip [0..] . map read . splitOn ","

data Mode = Pos | Imm | Rel deriving (Enum)

getModes :: Int -> [Mode]
getModes = (++ repeat Pos) . map (toEnum . digitToInt) . reverse . show

data Effect a = Input (Int -> a)
              | Output Int a deriving (Functor)
makeFree ''Effect

getMem :: Free Effect Memory -> Memory
getMem (Pure m) = m
getMem _ = error "Unable to read memory"

run :: Int -> Int -> Memory -> Free Effect Memory
run i rb mem =
    case instr of
      -- Addition
      1  -> run (i+4) rb $ (set 3 $ val 1 + val 2) mem
      -- Multiplication
      2  -> run (i+4) rb $ (set 3 $ val 1 * val 2) mem
      -- Store
      3  -> input >>= \v -> run (i+2) rb $ set 1 v mem
      -- Print
      4  -> output (val 1) >> (run (i+2) rb mem)
      -- Jump if true
      5  -> run (bool (i+3) (val 2) $ val 1 /= 0) rb mem
      -- Jump if false
      6  -> run (bool (i+3) (val 2) $ val 1 == 0) rb mem
      -- Less than
      7  -> run (i+4) rb $ (set 3 $ bool 0 1 $ val 1 < val 2) mem
      -- Equal
      8  -> run (i+4) rb $ (set 3 $ bool 0 1 $ val 1 == val 2) mem
      -- Adjust relative base
      9  -> run (i+2) (rb + val 1) mem
      -- Stop execution
      99 -> pure mem
      x  -> error $ "Unknown instruction: " ++ show x
    where arg n = mem ! (i + n)
          set a v = case modes !! (a - 1) of
                      Pos -> M.insert (arg a) v
                      Imm -> error "set on immediate"
                      Rel -> M.insert (arg a + rb) v
          val a = case modes !! (a - 1) of
                    Pos -> M.findWithDefault 0 (arg a) mem
                    Imm -> arg a
                    Rel -> M.findWithDefault 0 (arg a + rb) mem
          instr = mem ! i `mod` 100
          modes = getModes $ mem ! i `div` 100

runV1 :: Int -> Int -> Memory -> Int
runV1 a b = (! 0) . getMem . run 0 0 . M.insert 1 a . M.insert 2 b

runV2 :: [Int] -> Memory -> [Int]
runV2 inp = runEffects inp . run 0 0
    where runEffects xss (Free rest) =
              case rest of
                Input f | x:xs <- xss -> runEffects xs $ f x
                        | otherwise -> error "Not enough inputs"
                Output v eff -> v : runEffects xss eff
          runEffects _ (Pure _) = []
