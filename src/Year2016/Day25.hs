{-# LANGUAGE TupleSections, ViewPatterns #-}

module Year2016.Day25
    ( part1
    , part2
    ) where

import Year2016.Day25h

import Control.Lens ((%=), (.=), (+=), (-=), _2, assign, ix, over, use, view)
import Control.Monad (when)
import Control.Monad.Extra (whenM)
import Control.Monad.State.Strict
import Data.Foldable (toList)
import Data.Maybe (fromJust, mapMaybe)
import Data.Sequence ((|>))
import qualified Data.Sequence as S
import Data.Vector ((!), Vector)
import qualified Data.Vector as V
import Text.Megaparsec ((<|>), eitherP, oneOf, parseMaybe, space, spaceChar, string, try)
import Text.Megaparsec.Lexer (integer, signed)
import Text.Megaparsec.String (Parser)
import Debug.Trace


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
          parseTgl  = string "out " >> Out <$> register

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
evalInstr (Out r)           = do
  v <- use $ reg r
  output %= (|> v)
  o <- use output
  when (length o > 10) $
       currentLine .= -100
evalInstr (Jnz v (Reg r))   = use (reg r) >>= \l -> evalInstr (Jnz v (Const l))
evalInstr (Jnz v (Const l)) = whenM ((/= 0) <$> val v) $ currentLine += l - 1
evalInstr (Mul a b c d) = do
  b' <- use (reg b)
  reg c += a * b'
  reg b .= 0
  reg d .= 0

evaluate :: State Simulator Bool
evaluate = do
  line <- use currentLine
  instrs <- use instructions
  if (line >= 0 && line < V.length instrs) then do
    evalInstr $ instrs ! line
    currentLine += 1
    evaluate
  else do
    o <- use output
    return $ and $ zipWith (==) (toList o) (cycle [0, 1])


optimize :: Vector Instruction -> Vector Instruction
optimize v = foldr replaceMul v
             $ mapMaybe (\i -> (i,) <$> matchesMul (V.slice i 6 v)) [0 .. V.length v - 6]
    where matchesMul (V.toList -> [ Cpy (Const a) d
                                  , Inc c
                                  , Dec d'
                                  , Jnz (Reg d'') (Const (-2))
                                  , Dec b
                                  , Jnz (Reg b') (Const (-5))])
              | d == d' && d' == d'' && b == b' = Just (a, b, c, d)
              | otherwise = Nothing
          matchesMul _ = Nothing
          toMul a b c d = V.fromList [ Mul a b c d
                                     , Jnz (Const 0) (Const 0)
                                     , Jnz (Const 0) (Const 0)
                                     , Jnz (Const 0) (Const 0)
                                     , Jnz (Const 0) (Const 0)
                                     , Jnz (Const 0) (Const 0)
                                     ]
          replaceMul (i,(a,b,c,d)) v = let (v1, (_, v2)) = over _2 (V.splitAt 6) $ V.splitAt i v
                                       in V.concat [v1, toMul a b c d, v2]


findA :: Vector Instruction -> Int
findA instrs = go 0
    where go i
              | evalState evaluate (Sim i 0 0 0 0 S.empty instrs) = i
              | otherwise = go (i+1)

part1 :: String -> String
part1 = show . findA . optimize . parseInput

part2 :: String -> String
part2 = const ""
