module Year2019.IntCode where

import Data.Bool
import Data.Char
import Data.List.Split (splitOn)
import Data.Vector.Unboxed (Vector, (!))
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV


type Memory = Vector Int

parse :: String -> Memory
parse = V.fromList . map read . splitOn ","

revDigits :: Int -> [Int]
revDigits = (++ repeat 0) . map digitToInt . reverse . show

data Effect = Halt Memory
            | Input (Int -> Effect)
            | Output Int Effect

getMem :: Effect -> Memory
getMem (Halt m) = m
getMem _ = error "Unable to read memory"

run :: Int -> Memory -> Effect
run i mem =
    case instr of
      -- Addition
      1  -> run (i+4) $ (set 3 $ val 1 + val 2) mem
      -- Multiplication
      2  -> run (i+4) $ (set 3 $ val 1 * val 2) mem
      -- Store
      3  -> Input $ \v -> run (i+2) $ set 1 v mem
      -- Print
      4  -> Output (val 1) (run (i+2) mem)
      -- Jump if true
      5  -> run (bool (i+3) (val 2) $ val 1 /= 0) mem
      -- Jump if false
      6  -> run (bool (i+3) (val 2) $ val 1 == 0) mem
      -- Less than
      7  -> run (i+4) $ (set 3 $ bool 0 1 $ val 1 < val 2) mem
      -- Equal
      8  -> run (i+4) $ (set 3 $ bool 0 1 $ val 1 == val 2) mem
      -- Stop execution
      99 -> Halt mem
      x  -> error $ "Unknown instruction: " ++ show x
    where arg n = mem ! (i + n)
          set a v = if modes !! (a - 1) == 0
                    then V.modify $ \vec -> MV.write vec (arg a) v
                    else error "set on immediate"
          val a = if modes !! (a - 1) == 0
                  then mem ! arg a
                  else arg a
          instr = mem ! i `mod` 100
          modes = revDigits $ mem ! i `div` 100

runV1 :: Int -> Int -> Memory -> Int
runV1 a b = (! 0) . getMem . run 0 . (V.// [(1, a), (2, b)])

runV2 :: [Int] -> Memory -> [Int]
runV2 input = runEffects input . run 0
    where runEffects xss =
              \case
               Input f | x:xs <- xss -> runEffects xs $ f x
                       | otherwise -> error "Not enough inputs"
               Output v eff -> v : runEffects xss eff
               Halt _ -> []
