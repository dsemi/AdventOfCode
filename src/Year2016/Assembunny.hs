{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Year2016.Assembunny
    ( Simulator(..)
    , regs
    , evaluate
    , evaluateOutput
    , parseInstructions
    ) where

import Utils

import Data.Array
import Control.Lens
import Control.Monad (guard, void)
import Data.Maybe (fromJust, listToMaybe, mapMaybe)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Pipes
import Text.Megaparsec (eitherP, parseMaybe, (<|>))
import Text.Megaparsec.Char (oneOf, space, spaceChar, string)
import Text.Megaparsec.Char.Lexer (decimal, signed)


type Register = Char
type Value = Either Register Int

data Instruction = Cpy Value Value
                 | Inc Char
                 | Dec Char
                 | Tgl Char
                 | Out Value
                 | Jnz Value Value deriving (Eq, Show)

data Optimization = Peephole { apply :: Vector Instruction -> Maybe (Simulator -> Simulator)
                             }

data Simulator = Sim { _regs :: Array Char Int
                     , _line :: Int
                     , _optimizations :: [Optimization]
                     , _instrs :: Vector Instruction
                     }
makeLenses ''Simulator

data Actions m = Actions { transmit :: Int -> m () }

parseInstructions :: String -> Simulator
parseInstructions = Sim (listArray ('a', 'd') $ repeat 0) 0
                    [multiplication, plusEquals] . V.fromList
                    . map (fromJust . parseMaybe parseInstruction) . lines
    where parseInstruction :: Parser Instruction
          parseInstruction = parseCpy
                             <|> parseInc
                             <|> parseDec
                             <|> parseTgl
                             <|> parseOut
                             <|> parseJnz
          int = signed space $ fromInteger <$> decimal
          register = oneOf "abcd"
          value = eitherP register int
          parseCpy = string "cpy " >> Cpy <$> value <* spaceChar <*> value
          parseInc = string "inc " >> Inc <$> register
          parseDec = string "dec " >> Dec <$> register
          parseTgl = string "tgl " >> Tgl <$> register
          parseOut = string "out " >> Out <$> value
          parseJnz = string "jnz " >> Jnz <$> value <* spaceChar <*> value

val :: Simulator -> Value -> Int
val sim = either (\r -> sim ^?! (regs . ix r)) id

reg :: Applicative f => Char -> (Int -> f Int) -> Simulator -> f Simulator
reg c = regs . ix c

multiplication :: Optimization
multiplication = Peephole $ \is -> do
  guard $ V.length is >= 6
  Cpy a (Left d) <- pure $ V.unsafeIndex is 0
  Inc c <- pure $ V.unsafeIndex is 1
  Dec ((== d) -> True) <- pure $ V.unsafeIndex is 2
  Jnz (Left ((== d) -> True)) (Right (-2)) <- pure $ V.unsafeIndex is 3
  Dec b <- pure $ V.unsafeIndex is 4
  Jnz (Left ((== b) -> True)) (Right (-5)) <- pure $ V.unsafeIndex is 5
  pure $ \sim ->
      sim & reg c +~ (val sim a * val sim (Left b))
          & reg b .~ 0
          & reg d .~ 0
          & line +~ 6

plusEquals :: Optimization
plusEquals = Peephole $ \is -> do
  guard $ V.length is >= 3
  Inc a <- pure $ V.unsafeIndex is 0
  Dec b <- pure $ V.unsafeIndex is 1
  Jnz (Left ((== b) -> True)) (Right (-2)) <- pure $ V.unsafeIndex is 2
  pure $ \sim ->
      sim & reg a +~ val sim (Left b)
          & reg b .~ 0
          & line +~ 3

evalNextInstr :: (Monad m) => Actions m -> Simulator -> m (Maybe Simulator)
evalNextInstr (Actions {transmit}) sim =
    case maybeOptimize $ V.drop cl $ sim ^. instrs of
      Nothing -> traverse eval $ sim ^? (instrs . ix cl)
      x -> pure x
    where cl = sim  ^. line
          value = val sim
          maybeOptimize is = listToMaybe
                             $ mapMaybe (fmap ($ sim) . ($ is) . apply)
                             $ sim ^. optimizations
          eval (Cpy a b) = pure $ sim & either (\r -> reg r .~ value a) (const id) b & line +~ 1
          eval (Inc r)   = pure $ sim & reg r %~ succ & line +~ 1
          eval (Dec r)   = pure $ sim & reg r %~ pred & line +~ 1
          eval (Tgl r)   = pure $ sim & over (instrs . ix (value (Left r) + cl)) tgl & line +~ 1
          eval (Out v)   = do
            transmit $ value v
            pure $ sim & line +~ 1
          eval (Jnz a b) = pure $ sim & line +~ (if value a /= 0 then value b else 1)
          tgl (Cpy a b) = Jnz a b
          tgl (Inc r)   = Dec r
          tgl (Dec r)   = Inc r
          tgl (Jnz a b) = Cpy a b
          tgl (Tgl r)   = Inc r
          tgl _ = error "Invalid toggle"

run :: (Monad m) => Actions m -> Simulator -> m Simulator
run actions = go
    where go sim = do
            sim' <- evalNextInstr actions sim
            case sim' of
              Just s  -> go s
              Nothing -> pure sim

evaluate :: Simulator -> Simulator
evaluate = runIdentity . run actions
    where actions = Actions { transmit = \_ -> pure () }

evaluateOutput :: (Monad m) => Simulator -> Producer Int m ()
evaluateOutput sim = void (run actions sim)
    where actions = Actions { transmit = yield }
