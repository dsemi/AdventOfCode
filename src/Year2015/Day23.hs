{-# LANGUAGE FlexibleContexts, StrictData, TemplateHaskell #-}

module Year2015.Day23
    ( part1
    , part2
    ) where

import Control.Lens
import Control.Monad.Cont
import Control.Monad.Extra
import Control.Monad.Ref
import Control.Monad.State
import Control.Monad.ST
import Data.Maybe
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import Data.STRef
import Text.Megaparsec
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
                           }
makeLenses ''Simulator

parseInstructions :: String -> [Instruction]
parseInstructions = map (fromJust . parseMaybe parseInstruction) . lines
    where parseInstruction :: Parsec () String Instruction
          parseInstruction = parseHlf
                             <|> parseTpl
                             <|> parseInc
                             <|> parseJmp
                             <|> parseJie
                             <|> parseJio
          int = signed space $ fromInteger <$> decimal
          parseHlf :: Parsec () String Instruction
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

type Kont m = () -> m ()
goto :: Kont m -> m ()
goto k = k ()

go :: (MonadState Simulator m, MonadCont m, MonadRef ref m) =>
      ref (HashMap Int (Kont m)) -> Kont m -> [(Int, Instruction)] -> m ()
go _ _ [] = pure ()
go labels kont ((i, instr) : xs) = do
  callCC $ \k -> do
    modifyRef' labels (M.insert i k)
    go labels kont xs
  let follow o = readRef labels >>= goto . M.lookupDefault kont (i + o)
  case instr of
    Hlf r -> reg r %= (`div` 2)
    Tpl r -> reg r *= 3
    Inc r -> reg r += 1
    Jmp o -> follow o
    Jie r o -> whenM (even <$> use (reg r)) $ follow o
    Jio r o -> whenM ((== 1) <$> use (reg r)) $ follow o

run :: Int -> [Instruction] -> Int
run a' instrs = runST $ do
  labels <- newSTRef M.empty
  flip evalStateT (Simulator a' 0) $ flip runContT (const $ use b)
           $ callCC $ \k -> go labels k $ reverse $ zip [0..] instrs

part1 :: String -> Int
part1 = run 0 . parseInstructions

part2 :: String -> Int
part2 = run 1 . parseInstructions
