module Advent.Day23
    ( part1
    , part2
    ) where

import Advent.Problem

import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Char8 (pack)
import Data.Either
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import qualified Data.Vector as V

data Instruction = Cmd { inst :: String
                       , reg :: String
                       , off :: String
                       } deriving (Show)

instr :: Parser Instruction
instr = do
  i <- many1 letter_ascii <* space
  r <- many1 $ notChar ','
  o <- option "" $ char ',' *> space *> many1 (notChar ' ')
  return $ Cmd i r o

signedInt :: Parser Int
signedInt = signed decimal

cmd mem (Cmd i r o)
    | i == "hlf"           = M.adjust (`div` 2) r mem'
    | i == "tpl"           = M.adjust (*3) r mem'
    | i == "inc"           = M.adjust (+1) r mem'
    | i == "jmp"           = M.adjust (+r') "addr" mem
    | i == "jie" && even v = M.adjust (+o') "addr" mem
    | i == "jio" && v == 1 = M.adjust (+o') "addr" mem
    | otherwise            = mem'
    where mem' = M.adjust (+1) "addr" mem
          v    = mem M.! r
          (Right r') = parseOnly signedInt $ pack r
          (Right o') = parseOnly signedInt $ pack o

run mem instrs
    | 0 <= i && i < V.length instrs = run (cmd mem $ instrs V.! i) instrs
    | otherwise                     = mem
    where i = mem M.! "addr"

day23 :: HashMap String Int -> String -> Int
day23 mem = (M.! "b") . run mem . V.fromList . rights . map (parseOnly instr . pack) . lines

part1 :: Problem
part1 = Pure $ day23 mem
    where mem = M.fromList [("a", 0), ("b", 0), ("addr", 0)]

part2 :: Problem
part2 = Pure $ day23 mem
    where mem = M.fromList [("a", 1), ("b", 0), ("addr", 0)]
