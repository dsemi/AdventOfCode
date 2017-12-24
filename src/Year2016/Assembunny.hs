{-# LANGUAGE StrictData, TemplateHaskell, TupleSections, ViewPatterns #-}

module Year2016.Assembunny
    ( Simulator(..)
    , regs
    , output
    , instructions
    , evaluate
    , evaluateUntilOutputLengthIs
    , parseInstructions
    ) where

import Utils

import Data.Array
import Control.Lens hiding ((|>))
import Control.Monad (guard)
import Data.Function ((&))
import Data.Maybe (fromJust)
import Data.Sequence (Seq, empty, (|>))
import Data.Vector (Vector)
import qualified Data.Vector as V
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

data Simulator = Sim { _regs :: Array Char Int
                     , _currentLine :: Int
                     , _output :: Seq Int
                     , _instructions :: Vector Instruction
                     } deriving (Eq, Show)

makeLenses ''Simulator

parseInstructions :: String -> Simulator
parseInstructions = Sim (listArray ('a', 'd') $ repeat 0) 0 empty . V.fromList
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

multiplication :: Vector Instruction -> Maybe (Value, Char, Char, Char)
multiplication instrs = do
  guard $ V.length instrs >= 6
  Cpy a (Left d) <- pure $ V.unsafeIndex instrs 0
  Inc c <- pure $ V.unsafeIndex instrs 1
  Dec ((== d) -> True) <- pure $ V.unsafeIndex instrs 2
  Jnz (Left ((== d) -> True)) (Right (-2)) <- pure $ V.unsafeIndex instrs 3
  Dec b <- pure $ V.unsafeIndex instrs 4
  Jnz (Left ((== b) -> True)) (Right (-5)) <- pure $ V.unsafeIndex instrs 5
  pure (a, b, c, d)

plusEquals :: Vector Instruction -> Maybe (Char, Char)
plusEquals instrs = do
  guard $ V.length instrs >= 3
  Inc a <- pure $ V.unsafeIndex instrs 0
  Dec b <- pure $ V.unsafeIndex instrs 1
  Jnz (Left ((== b) -> True)) (Right (-2)) <- pure $ V.unsafeIndex instrs 2
  pure (a, b)

evalNextInstr :: Simulator -> Simulator
evalNextInstr sim@(Sim{_currentLine=cl}) =
    let (sim', i) = eval $ V.unsafeDrop cl $ sim ^. instructions
    in sim' & currentLine +~ i
    where reg c = regs . ix c
          value :: Value -> Int
          value = either (\r -> sim ^?! reg r) id
          eval :: Vector Instruction -> (Simulator, Int)
          eval (multiplication -> Just (a, b, c, d)) =
              ( sim & reg c %~ (+ (value a * value (Left b)))
                    & reg b .~ 0
                    & reg d .~ 0
              , 6 )
          eval (plusEquals -> Just (a, b)) =
              ( sim & reg a %~ (+ value (Left b))
                    & reg b .~ 0
              , 3 )
          eval instrs =
              case V.unsafeHead instrs of
                (Cpy a b) -> ( sim & either (\r -> reg r .~ value a) (const id) b
                             , 1 )
                (Inc r)   -> ( sim & reg r %~ succ
                             , 1 )
                (Dec r)   -> ( sim & reg r %~ pred
                             , 1 )
                (Tgl r)   -> ( sim & over (instructions . ix (value (Left r) + cl)) tgl
                             , 1 )
                (Out v)   -> ( sim & output %~ (|> value v)
                             , 1 )
                (Jnz a b) -> ( sim
                             , if value a /= 0 then value b else 1 )
              where tgl (Cpy a b) = Jnz a b
                    tgl (Inc r)   = Dec r
                    tgl (Dec r)   = Inc r
                    tgl (Jnz a b) = Cpy a b
                    tgl (Tgl r)   = Inc r
                    tgl _ = error "Invalid toggle"

evaluateWhile :: (Simulator -> Bool) -> Simulator -> Simulator
evaluateWhile cond = go
    where go sim
              | cond sim  = go $ evalNextInstr sim
              | otherwise = sim

lineInBounds :: Simulator -> Bool
lineInBounds (Sim {_currentLine=line, _instructions=instrs}) =
    line >= 0 && line < V.length instrs

outLengthIs :: Int -> Simulator -> Bool
outLengthIs n (Sim {_output=out}) = length out <= n

evaluate :: Simulator -> Simulator
evaluate = evaluateWhile lineInBounds

evaluateUntilOutputLengthIs :: Int -> Simulator -> Simulator
evaluateUntilOutputLengthIs n =
    evaluateWhile ((&&) <$> outLengthIs n <*> lineInBounds)
