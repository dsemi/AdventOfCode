{-# LANGUAGE TupleSections, ViewPatterns #-}

module Year2016.Day23
    ( part1
    , part2
    ) where

import Year2016.Day23h

import Control.Lens ((.=), (+=), (-=), _2, assign, ix, over, use, view)
import Control.Monad (when)
import Control.Monad.Extra (whenM)
import Control.Monad.State.Strict
import Data.Maybe (fromJust, mapMaybe)
import Data.Vector ((!), Vector)
import qualified Data.Vector as V
import Text.Megaparsec ((<|>), eitherP, oneOf, parseMaybe, space, spaceChar, string, try)
import Text.Megaparsec.Lexer (integer, signed)
import Text.Megaparsec.String (Parser)

parseInput :: String -> Vector Instruction
parseInput = V.fromList . map (fromJust . parseMaybe parseInstruction) . lines
    where parseInstruction :: Parser Instruction
          parseInstruction = parseCpy
                             <|> parseInc
                             <|> parseDec
                             <|> parseJnz
                             <|> parseTgl
          int = signed space $ fromInteger <$> integer
          register = oneOf "abcd"
          toVal = fmap $ either Reg Const
          parseCpy  = string "cpy " >> Cpy <$> toVal (eitherP register int) <* spaceChar <*> register
          parseInc  = string "inc " >> Inc <$> register
          parseDec  = string "dec " >> Dec <$> register
          parseJnz  = string "jnz " >> Jnz <$> toVal (eitherP register int) <* spaceChar <*> toVal (eitherP register int)
          parseTgl  = string "tgl " >> Tgl <$> register

reg 'a' = a
reg 'b' = b
reg 'c' = c
reg 'd' = d

val :: Value -> State Simulator Int
val (Const i) = return i
val (Reg   r) = use $ reg r

evalInstr :: Instruction -> State Simulator ()
evalInstr (Cpy v r)         = val v >>= assign (reg r)
evalInstr (Inc r)           = reg r += 1
evalInstr (Dec r)           = reg r -= 1
evalInstr (Jnz v (Reg r))   = use (reg r) >>= \l -> evalInstr (Jnz v (Const l))
evalInstr (Jnz v (Const l)) = whenM ((/= 0) <$> val v) $ currentLine += l - 1
evalInstr (Noop _)          = return ()
evalInstr (Mul a b c d) = do
  a' <- use (reg a)
  b' <- use (reg b)
  reg c .= a' * b'
  reg b .= 0
  reg d .= 0
evalInstr (Tgl r) = do
  i <- use $ reg r
  instrs <- use instructions
  cl <- use currentLine
  when (i + cl < V.length instrs) $ do
    modify' $ over (instructions . ix (i+cl)) tgl
      where tgl (Cpy v r)       = Jnz v (Reg r)
            tgl (Inc r)         = Dec r
            tgl (Dec r)         = Inc r
            tgl (Jnz v (Reg r)) = Cpy v r
            tgl (Jnz v l)       = Noop (Just $ Jnz v l)
            tgl (Tgl r)         = Inc r
            tgl (Noop (Just i)) = i
            tgl (Noop Nothing)  = Noop Nothing

evaluate :: State Simulator ()
evaluate = do
  line <- use currentLine
  instrs <- use instructions
  when (line >= 0 && line < V.length instrs) $ do
   evalInstr $ instrs ! line
   currentLine += 1
   evaluate

part1 :: String -> Int
part1 = view a . execState evaluate . Sim 7 0 0 0 0 . optimize . parseInput

optimize :: Vector Instruction -> Vector Instruction
optimize v = foldr replaceMul v
             $ mapMaybe (\i -> (i,) <$> matchesMul (V.slice i 6 v)) [0 .. V.length v - 6]
    where matchesMul (V.toList -> [ Cpy (Reg a) d
                                  , Inc c
                                  , Dec d'
                                  , Jnz (Reg d'') (Const (-2))
                                  , Dec b
                                  , Jnz (Reg b') (Const (-5))])
              | d == d' && d' == d'' && b == b' = Just (a, b, c, d)
              | otherwise = Nothing
          matchesMul _ = Nothing
          toMul a b c d = V.fromList [ Mul a b c d
                                     , Noop Nothing
                                     , Noop Nothing
                                     , Noop Nothing
                                     , Noop Nothing
                                     , Noop Nothing
                                     ]
          replaceMul (i,(a,b,c,d)) v = let (v1, (_, v2)) = over _2 (V.splitAt 6) $ V.splitAt i v
                                       in V.concat [v1, toMul a b c d, v2]

part2 :: String -> Int
part2 = view a . execState evaluate . Sim 12 0 0 0 0 . optimize . parseInput
