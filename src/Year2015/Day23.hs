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
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import Data.STRef
import FlatParse.Basic

import Utils

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

parseInstructions :: ByteString -> [Instruction]
parseInstructions = map parse . B.lines
    where parse line = case runParser parseInstruction line of
                         OK i _ -> i
                         _ -> error "unreachable"
          parseInstruction = parseHlf
                             <|> parseTpl
                             <|> parseInc
                             <|> parseJmp
                             <|> parseJie
                             <|> parseJio
          letter = satisfy isLatinLetter
          parseHlf = $(string "hlf ") >> Hlf <$> letter
          parseTpl = $(string "tpl ") >> Tpl <$> letter
          parseInc = $(string "inc ") >> Inc <$> letter
          parseJmp = $(string "jmp ") >> Jmp <$> signedInt
          parseJie = $(string "jie ") >> Jie <$> letter <* $(string ", ") <*> signedInt
          parseJio = $(string "jio ") >> Jio <$> letter <* $(string ", ") <*> signedInt

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

part1 :: ByteString -> Int
part1 = run 0 . parseInstructions

part2 :: ByteString -> Int
part2 = run 1 . parseInstructions
