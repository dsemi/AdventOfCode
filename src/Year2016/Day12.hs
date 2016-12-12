module Year2016.Day12
    ( part1
    , part2
    ) where

import Year2016.Day12h

import Control.Lens ((+=), (-=), assign, use, view)
import Control.Monad (when)
import Control.Monad.Extra (whenM)
import Control.Monad.State (execState, State)
import Data.Maybe (mapMaybe)
import Data.Vector ((!), Vector)
import qualified Data.Vector as V
import Text.Megaparsec ((<|>), eitherP, oneOf, parseMaybe, space, spaceChar, string)
import Text.Megaparsec.Lexer (integer, signed)
import Text.Megaparsec.String (Parser)


parseInput :: String -> Vector Instruction
parseInput = V.fromList . mapMaybe (parseMaybe parseInstruction) . lines
    where parseInstruction :: Parser Instruction
          parseInstruction = parseCpy <|> parseInc <|> parseDec <|> parseJnz
          int = signed space $ fromInteger <$> integer
          register = oneOf "abcd"
          toVal = fmap $ either Reg Const
          parseCpy = string "cpy " >> Cpy <$> toVal (eitherP register int) <* spaceChar <*> register
          parseInc = string "inc " >> Inc <$> register
          parseDec = string "dec " >> Dec <$> register
          parseJnz = string "jnz " >> Jnz <$> toVal (eitherP register int) <* spaceChar <*> int

reg 'a' = a
reg 'b' = b
reg 'c' = c
reg 'd' = d

val :: Value -> State Simulator Int
val (Const i) = return i
val (Reg   r) = use $ reg r

evalInstr :: Instruction -> State Simulator ()
evalInstr (Cpy v r) = val v >>= assign (reg r)
evalInstr (Inc r)   = reg r += 1
evalInstr (Dec r)   = reg r -= 1
evalInstr (Jnz v l) = whenM ((/= 0) <$> val v) $ currentLine += l - 1

evaluate :: State Simulator ()
evaluate = do
  line <- use currentLine
  instrs <- use instructions
  when (line >= 0 && line < V.length instrs) $ do
   evalInstr $ instrs ! line
   currentLine += 1
   evaluate

part1 :: String -> Int
part1 = view a . execState evaluate . Sim 0 0 0 0 0 . parseInput

part2 :: String -> Int
part2 = view a . execState evaluate . Sim 0 0 1 0 0 . parseInput
