{-# LANGUAGE StrictData, TemplateHaskell, TupleSections, ViewPatterns #-}

module Year2016.Assembunny
    ( Simulator(..)
    , a, b, c, d, output, instructions
    , evaluate
    , evaluateUntilOutputLengthIs
    , parseInstructions
    ) where

import Utils

import Control.Lens (ix, over, set, view, (%~), (.~), (+~), (-~), (&))
import Control.Lens.TH (makeLenses)
import Control.Monad
import Control.Monad.ST (ST, runST)
import Data.Function ((&))
import Data.Maybe (fromJust, mapMaybe)
import Data.Sequence (Seq, empty, (|>))
import Data.STRef (modifySTRef', newSTRef, readSTRef)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Text.Megaparsec (eitherP, parseMaybe, (<|>))
import Text.Megaparsec.Char (oneOf, space, spaceChar, string)
import Text.Megaparsec.Char.Lexer (decimal, signed)


data Value = Const Int | Reg Char deriving (Eq, Show)

data Instruction = Cpy Value Value
                 | Inc Char
                 | Dec Char
                 | Tgl Char
                 | Out Char
                 | Jnz Value Value deriving (Eq, Show)

data Simulator = Sim { _a :: Int
                     , _b :: Int
                     , _c :: Int
                     , _d :: Int
                     , _currentLine :: Int
                     , _output :: Seq Int
                     , _instructions :: Vector Instruction
                     } deriving (Eq, Show)

makeLenses ''Simulator


parseInstructions :: String -> Simulator
parseInstructions = Sim 0 0 0 0 0 empty . V.fromList
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
          value = either Reg Const <$> eitherP register int
          parseCpy = string "cpy " >> Cpy <$> value <* spaceChar <*> value
          parseInc = string "inc " >> Inc <$> register
          parseDec = string "dec " >> Dec <$> register
          parseTgl = string "tgl " >> Tgl <$> register
          parseOut = string "out " >> Out <$> register
          parseJnz = string "jnz " >> Jnz <$> value <* spaceChar <*> value

reg 'a' = a
reg 'b' = b
reg 'c' = c
reg 'd' = d

value :: Value -> Simulator -> Int
value (Const i) _ = i
value (Reg   r) s = view (reg r) s

multiplication :: Vector Instruction -> Maybe (Value, Char, Char, Char)
multiplication instrs = case V.toList (V.unsafeTake 6 instrs) of
                          [ Cpy a (Reg d)
                           , Inc c
                           , Dec d'
                           , Jnz (Reg d'') (Const (-2))
                           , Dec b
                           , Jnz (Reg b') (Const (-5))
                           ] -> if d == d' && d' == d'' && b == b'
                                then Just (a, b, c, d)
                                else Nothing
                          _ -> Nothing

evalNextInstr :: Simulator -> Simulator
evalNextInstr sim@(Sim{_currentLine=cl}) = eval $ V.drop cl $ view instructions sim
    where eval :: Vector Instruction -> Simulator
          eval (multiplication -> Just (a, b, c, d)) =
            let a' = value a sim
                b' = view (reg b) sim
            in sim & reg c +~ (a' * b')
                   & reg b .~ 0
                   & reg d .~ 0
                   & currentLine +~ 6
          eval instrs =
              case V.unsafeHead instrs of
                (Cpy v v') ->
                  let f = case v' of
                            (Reg r)   -> reg r .~ value v sim
                            (Const _) -> id
                  in sim & f
                         & currentLine +~ 1
                (Inc r) ->
                  sim & reg r +~ 1
                      & currentLine +~ 1
                (Dec r) ->
                  sim & reg r -~ 1
                      & currentLine +~ 1
                (Tgl r) ->
                  let i = view (reg r) sim
                      f = if i + cl < V.length (view instructions sim)
                          then over (instructions . ix (i+cl)) tgl
                          else id
                  in sim & f
                         & currentLine +~ 1
                (Out r) ->
                  let v = view (reg r) sim
                  in sim & output %~ (|> v)
                         & currentLine +~ 1
                (Jnz v v') ->
                  let l = value v' sim
                      rv = value v sim
                  in sim & if rv /= 0
                           then currentLine +~ l
                           else currentLine +~ 1
              where tgl (Cpy v v') = Jnz v v'
                    tgl (Inc r)    = Dec r
                    tgl (Dec r)    = Inc r
                    tgl (Jnz v v') = Cpy v v'
                    tgl (Tgl r)    = Inc r


evaluateWhile :: (Simulator -> Bool) -> Simulator -> ST s Simulator
evaluateWhile f s = do
  ref <- newSTRef s
  go ref
  readSTRef ref
    where go ref = do
            cond <- f <$> readSTRef ref
            when cond $ do
              modifySTRef' ref evalNextInstr
              go ref

lineInBounds :: Simulator -> Bool
lineInBounds (Sim {_currentLine=line, _instructions=instrs}) =
    line >= 0 && line < V.length instrs

outLengthIs :: Int -> Simulator -> Bool
outLengthIs n (Sim {_output=out}) = length out <= n

evaluate :: Simulator -> Simulator
evaluate sim = runST $ evaluateWhile lineInBounds sim

evaluateUntilOutputLengthIs :: Int -> Simulator -> Simulator
evaluateUntilOutputLengthIs n sim =
    runST (evaluateWhile ((&&) <$> outLengthIs n <*> lineInBounds) sim)
