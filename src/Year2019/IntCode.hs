{-# LANGUAGE FlexibleContexts, TemplateHaskell #-}

module Year2019.IntCode
    ( Action(..)
    , Memory
    , parse
    , runNoIO
    , runIO
    , runProg
    , runPure
    , runWithInput
    ) where

import Control.Lens (view)
import Control.Monad
import Control.Monad.Free
import Control.Monad.Free.TH
import Data.Bool
import qualified Data.ByteString as B
import Data.List.Split (splitOn)
import Data.IntMap.Strict (IntMap, (!))
import qualified Data.IntMap.Strict as M
import Data.Traversable
import Pipes
import qualified Pipes.ByteString as PB
import qualified Pipes.Prelude as P


type Memory = IntMap Int

parse :: String -> Memory
parse = M.fromList . zip [0..] . map read . splitOn ","

data Action a = Input (Int -> a)
              | Output Int a
                deriving (Functor)
makeFree ''Action

data Mode = Pos | Imm | Rel deriving (Enum)

data Instr a = Add a a a
             | Mul a a a
             | Sav a
             | Out a
             | Jit a a
             | Jif a a
             | Lt  a a a
             | Eql a a a
             | Arb a
             | Hlt
               deriving (Functor, Foldable, Traversable)

parseInstr :: Int -> Memory -> (Int, Instr (Mode, Int))
parseInstr idx mem = (idx + len, instr)
    where (len, instr) =
              mapAccumL (\i _ -> (i + 1, (mode i, idx + i))) 1 $
              case mem ! idx `mod` 100 of
                1  -> Add () () ()
                2  -> Mul () () ()
                3  -> Sav ()
                4  -> Out ()
                5  -> Jit () ()
                6  -> Jif () ()
                7  -> Lt  () () ()
                8  -> Eql () () ()
                9  -> Arb ()
                99 -> Hlt
                _  -> error $ "Unknown op code: " ++ show (mem ! idx)
          mode i = toEnum (mem ! idx `div` 10^(i+1) `mod` 10)

run :: Int -> Int -> Memory -> Free Action Int
run idx rb mem =
    case applyMode <$> instr of
      Add a b c -> run idx' rb $ M.insert c (val a + val b) mem
      Mul a b c -> run idx' rb $ M.insert c (val a * val b) mem
      Sav a     -> input >>= \v -> run idx' rb $ M.insert a v mem
      Out a     -> output (val a) >> run idx' rb mem
      Jit a b   -> run (if val a /= 0 then val b else idx') rb mem
      Jif a b   -> run (if val a == 0 then val b else idx') rb mem
      Lt  a b c -> run idx' rb $ M.insert c (bool 0 1 $ val a < val b) mem
      Eql a b c -> run idx' rb $ M.insert c (bool 0 1 $ val a == val b) mem
      Arb a     -> run idx' (rb + val a) mem
      Hlt       -> pure $ mem ! 0
    where (idx', instr) = parseInstr idx mem
          val i = M.findWithDefault 0 i mem
          applyMode (mode, i) =
              case mode of
                Pos -> val i
                Imm -> i
                Rel -> val i + rb

runPure :: Memory -> Free Action ()
runPure = void . run 0 0

runNoIO :: Int -> Int -> Memory -> Int
runNoIO a b = iter undefined . run 0 0 . M.insert 1 a . M.insert 2 b

runProg :: (Monad m) => Memory -> Pipe Int Int m ()
runProg = foldFree eval . runPure
    where eval (Input k) = k <$> await
          eval (Output i k) = yield i >> pure k

runWithInput :: [Int] -> Memory -> [Int]
runWithInput inp mem = P.toList $ each inp >-> runProg mem

runIO :: Memory -> IO ()
runIO mem = runEffect $
            view PB.unpack PB.stdin
            >-> P.map fromEnum
            >-> runProg mem
            >-> P.map (B.singleton . toEnum)
            >-> PB.stdout
