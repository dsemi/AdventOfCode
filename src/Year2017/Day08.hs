{-# LANGUAGE NamedFieldPuns #-}

module Year2017.Day08
    ( part1
    , part2
    ) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M


data Instr = Instr { cmd :: HashMap String Int -> HashMap String Int
                   , cond :: HashMap String Int -> Bool
                   }

op :: Num a => [Char] -> a -> a -> a
op "inc" = (+)
op "dec" = subtract
op _ = error "Invalid op"

comp :: Ord a => [Char] -> a -> a -> Bool
comp "!=" = (/=)
comp "==" = (==)
comp ">=" = (>=)
comp ">"  = (>)
comp "<=" = (<=)
comp "<"  = (<)
comp _ = error "Invalid comp"

parseInstrs :: String -> [Instr]
parseInstrs = map parseInstr . lines
    where parseInstr :: String -> Instr
          parseInstr line =
              let [reg, fn, amnt, "if", reg2, cmp, val] = words line
              in Instr { cmd = \m -> M.insert reg (op fn (read amnt) $ M.lookupDefault 0 reg m) m
                       , cond = \m -> comp cmp (M.lookupDefault 0 reg2 m) $ read val
                       }

eval :: HashMap String Int -> Instr -> HashMap String Int
eval m (Instr {cmd, cond}) = if cond m then cmd m else m

part1 :: String -> Int
part1 = maximum . foldl eval M.empty . parseInstrs

part2 :: String -> Int
part2 = maximum . concatMap M.elems . scanl eval M.empty . parseInstrs
