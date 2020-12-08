{-# LANGUAGE FlexibleContexts #-}

module Year2020.Day08
    ( part1
    , part2
    ) where

import Control.Lens
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Set as S
import Data.Vector (Vector, (!?))
import qualified Data.Vector as V


data Instr = Acc Int
           | Jmp Int
           | Nop Int deriving (Eq)

toggle :: Instr -> Instr
toggle (Acc n) = Acc n
toggle (Jmp n) = Nop n
toggle (Nop n) = Jmp n

parseInstrs :: String -> Vector Instr
parseInstrs = V.fromList . map parse . lines
    where parseInt d = read $ if head d == '+' then tail d else d
          parse line = case words line of
                         ["acc", d] -> Acc (parseInt d)
                         ["jmp", d] -> Jmp (parseInt d)
                         ["nop", d] -> Nop (parseInt d)
                         _ -> error "bad parse"

run :: Vector Instr -> Either Int Int
run instrs = (`evalState` S.empty) $ runExceptT $ go 0 0
    where go :: (MonadState (S.Set Int) m, MonadError Int m) => Int -> Int -> m Int
          go acc i = do
            visited <- get
            when (S.member i visited) $ throwError acc
            modify' $ S.insert i
            case instrs !? i of
              Just (Acc n) -> go (acc + n) (i + 1)
              Just (Jmp n) -> go acc (i+n)
              Just (Nop _) -> go acc (i+1)
              Nothing -> pure acc

part1 :: String -> Either Int Int
part1 = run . parseInstrs

part2 :: String -> Int
part2 (parseInstrs -> instrs) = head [ n | i <- [0.. V.length instrs - 1]
                                     , Right n <- [run $ over (ix i) toggle instrs]]
