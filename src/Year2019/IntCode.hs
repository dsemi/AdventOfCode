module Year2019.IntCode
    ( Memory
    , parse
    , runNoIO
    , runPipe
    , runWithInput
    ) where

import Data.Bool
import Data.Functor.Identity
import Data.List.Split (splitOn)
import Data.Maybe
import Data.IntMap.Strict (IntMap, (!))
import qualified Data.IntMap.Strict as M
import Pipes
import qualified Pipes.Prelude as P


type Memory = IntMap Int

data Program = Program { idx :: !Int
                       , relBase :: !Int
                       , memory :: !Memory
                       }

parse :: String -> Memory
parse = M.fromList . zip [0..] . map read . splitOn ","

build :: Memory -> Program
build = Program 0 0

data Instr = Add | Mul | Sav | Out | Jit | Jif | Lt | Eql | Arb | Hlt

toInstr :: Int -> Instr
toInstr i = fromMaybe (error $ "Unknown op code: " ++ show i)
            $ lookup i [ (1, Add), (2, Mul), (3, Sav), (4, Out), (5, Jit)
                       , (6, Jif), (7,  Lt), (8, Eql), (9, Arb), (99, Hlt) ]

data Mode = Pos | Imm | Rel deriving (Enum)

run :: (Monad m) => (m Int) -> (Int -> m ()) -> Program -> m Program
run input output = go
    where go prog = case toInstr op of
                      Add -> go $ inc 4 $ set 3 (val 1 + val 2) prog
                      Mul -> go $ inc 4 $ set 3 (val 1 * val 2) prog
                      Sav -> input >>= \v -> go $ inc 2 $ set 1 v prog
                      Out -> output (val 1) >> go (inc 2 prog)
                      Jit -> go $ bool (inc 3) (jmp $ val 2) (val 1 /= 0) prog
                      Jif -> go $ bool (inc 3) (jmp $ val 2) (val 1 == 0) prog
                      Lt  -> go $ inc 4 $ set 3 (bool 0 1 $ val 1 < val 2) prog
                      Eql -> go $ inc 4 $ set 3 (bool 0 1 $ val 1 == val 2) prog
                      Arb -> go $ inc 2 $ incRb (val 1) prog
                      Hlt -> pure prog
              where arg n = get $ idx prog + n
                    inc n p = p {idx=idx p + n}
                    jmp n p = p {idx=n}
                    incRb n p = p {relBase=relBase p + n}
                    get i = M.findWithDefault 0 i $ memory prog
                    set a v p = case mode a of
                                  Pos -> p {memory=M.insert (arg a) v (memory p)}
                                  Imm -> error "set on immediate"
                                  Rel -> p {memory=M.insert (arg a + relBase p) v (memory p)}
                    val a = case mode a of
                              Pos -> get $ arg a
                              Imm -> arg a
                              Rel -> get $ arg a + relBase prog
                    op = get (idx prog) `mod` 100
                    mode i = toEnum $ get (idx prog) `div` 10^(i+1) `mod` 10

runNoIO :: Int -> Int -> Memory -> Int
runNoIO a b = (! 0) . memory . runIdentity . run (error "Unsupported") (error "Unsupported")
              . build . M.insert 1 a . M.insert 2 b

runWithInput :: [Int] -> Memory -> [Int]
runWithInput input mem = P.toList $ each input >-> void (run await yield $ build mem)

runPipe :: (Monad m) => Memory -> Pipe Int Int m ()
runPipe = void . run await yield . build
