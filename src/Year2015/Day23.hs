{-# LANGUAGE StrictData, TemplateHaskell #-}

module Year2015.Day23
    ( part1
    , part2
    ) where

import Utils

import Control.Lens (set, use, uses, view, (%=), (+=), (*=))
import Control.Lens.TH (makeLenses)
import Control.Monad.State.Strict
import Data.Maybe
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import Text.Megaparsec (parseMaybe, (<|>))
import Text.Megaparsec.Char (letterChar, space, string)
import Text.Megaparsec.Char.Lexer (decimal, signed)


data Instruction = Hlf Char
                 | Tpl Char
                 | Inc Char
                 | Jmp Int
                 | Jie Char Int
                 | Jio Char Int

data Simulator = Simulator { _a :: Int
                           , _b :: Int
                           , _currentLine :: Int
                           , _instructions :: Vector Instruction
                           }
makeLenses ''Simulator

parseInstructions :: String -> Simulator
parseInstructions = Simulator 0 0 0 . V.fromList
                    . map (fromJust . parseMaybe parseInstruction) . lines
    where parseInstruction :: Parser Instruction
          parseInstruction = parseHlf
                             <|> parseTpl
                             <|> parseInc
                             <|> parseJmp
                             <|> parseJie
                             <|> parseJio
          int = signed space $ fromInteger <$> decimal
          parseHlf :: Parser Instruction
          parseHlf = string "hlf " >> Hlf <$> letterChar
          parseTpl = string "tpl " >> Tpl <$> letterChar
          parseInc = string "inc " >> Inc <$> letterChar
          parseJmp = string "jmp " >> Jmp <$> int
          parseJie = string "jie " >> Jie <$> letterChar <* string ", " <*> int
          parseJio = string "jio " >> Jio <$> letterChar <* string ", " <*> int

reg :: Functor f => Char -> (Int -> f Int) -> Simulator -> f Simulator
reg 'a' = a
reg 'b' = b
reg  _  = error "Invalid register"

eval :: Instruction -> State Simulator (Int -> Int)
eval (Hlf r) = do
  reg r %= (`div` 2)
  return (+1)
eval (Tpl r) = do
  reg r *= 3
  return (+1)
eval (Inc r) = do
  reg r += 1
  return (+1)
eval (Jmp o) = do
  return (+o)
eval (Jie r o) = do
  v <- use $ reg r
  return $ if even v then (+o) else (+1)
eval (Jio r o) = do
  v <- use $ reg r
  return $ if v == 1 then (+o) else (+1)

evaluateWhile :: State Simulator Bool -> State Simulator ()
evaluateWhile f = do
  line <- use currentLine
  cond <- f
  when cond $ do
    instr <- uses instructions (! line)
    cl <- eval instr
    currentLine %= cl
    evaluateWhile f

lineInBounds :: State Simulator Bool
lineInBounds = do
  instrs <- use instructions
  line <- use currentLine
  return $ line >= 0 && line < V.length instrs

part1 :: String -> Int
part1 = view b . execState (evaluateWhile lineInBounds) . parseInstructions

part2 :: String -> Int
part2 = view b . execState (evaluateWhile lineInBounds) . set a 1 . parseInstructions
