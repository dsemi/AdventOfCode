module Year2016.Day12
    ( part1
    , part2
    ) where

import Year2016.Day12h

import Control.Lens
import Control.Monad.State
import Data.Maybe (mapMaybe)
import Data.Vector ((!), Vector)
import qualified Data.Vector as V
import Text.Megaparsec ((<|>), eitherP, letterChar, parseMaybe, space, spaceChar, string)
import Text.Megaparsec.Lexer (integer, signed)
import Text.Megaparsec.String (Parser)


parseInput :: String -> Vector Instruction
parseInput = V.fromList . mapMaybe (parseMaybe parseInstruction) . lines
    where parseInstruction :: Parser Instruction
          parseInstruction = parseCpy <|> parseInc <|> parseDec <|> parseJnz
          int = signed space $ fromInteger <$> integer
          parseCpy = Cpy <$> (string "cpy " *> eitherP letterChar int) <*> (spaceChar *> letterChar)
          parseInc = Inc <$> (string "inc " *> letterChar)
          parseDec = Dec <$> (string "dec " *> letterChar)
          parseJnz = Jnz <$> (string "jnz " *> eitherP letterChar int) <*> (spaceChar *> int)

cToR 'a' = a
cToR 'b' = b
cToR 'c' = c
cToR 'd' = d

evalInstr :: Instruction -> State Simulator ()
evalInstr (Cpy (Left c) r)  = modify $ \s -> set (cToR r) (s ^. (cToR c)) s
evalInstr (Cpy (Right i) r) = modify $ set (cToR r) i
evalInstr (Inc r)           = modify $ over (cToR r) (+1)
evalInstr (Dec r)           = modify $ over (cToR r) (subtract 1)
evalInstr (Jnz (Left c) l)  = do
  i <- (^. (cToR c)) <$> get
  when (i /= 0) $ modify $ over currentLine (+(l-1))
evalInstr (Jnz (Right i) l) = when (i /= 0) $ modify $ over currentLine (+(l-1))

evaluate :: State Simulator ()
evaluate = do
  s <- get
  let line = s ^. currentLine
  unless (line < 0 || line >= V.length (s ^. instrs)) $ do
   evalInstr $ (s ^. instrs) ! line
   modify $ over currentLine (+1)
   evaluate

part1 :: String -> Int
part1 = (^. a) . execState evaluate . Sim 0 0 0 0 0 . parseInput

part2 :: String -> Int
part2 = (^. a) . execState evaluate . Sim 0 0 1 0 0 . parseInput
