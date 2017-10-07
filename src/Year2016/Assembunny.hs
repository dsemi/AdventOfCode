{-# LANGUAGE StrictData, TemplateHaskell, TupleSections, ViewPatterns #-}

module Year2016.Assembunny
    ( Simulator(..)
    , a, b, c, d, output, instructions
    , evaluate
    , evaluateUntilOutputLengthIs
    , parseInstructions
    ) where

import Utils

import Control.Lens (ix, over, (^.), (%~), (.~), (+~), (-~))
import Control.Lens.TH (makeLenses)
import Control.Monad (guard, when)
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


data Value = Reg Char | Const Int deriving (Eq, Show)

data Instruction = Cpy Value Value
                 | Inc Char
                 | Dec Char
                 | Tgl Char
                 | Out Value
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
          parseOut = string "out " >> Out <$> value
          parseJnz = string "jnz " >> Jnz <$> value <* spaceChar <*> value

reg 'a' = a
reg 'b' = b
reg 'c' = c
reg 'd' = d

either' :: (Char -> a) -> (Int -> a) -> Value -> a
either' f g v = case v of
                  (Reg   c) -> f c
                  (Const i) -> g i

multiplication :: Vector Instruction -> Maybe (Value, Char, Char, Char)
multiplication instrs = do
  guard $ V.length instrs >= 6
  Cpy a (Reg d) <- return $ V.unsafeIndex instrs 0
  Inc c <- return $ V.unsafeIndex instrs 1
  Dec d' <- return $ V.unsafeIndex instrs 2
  guard $ d == d'
  Jnz (Reg d'') (Const (-2)) <- return $ V.unsafeIndex instrs 3
  guard $ d' == d''
  Dec b <- return $ V.unsafeIndex instrs 4
  Jnz (Reg b') (Const (-5)) <- return $ V.unsafeIndex instrs 5
  guard $ b == b'
  Just (a, b, c, d)

plusEquals :: Vector Instruction -> Maybe (Char, Char)
plusEquals instrs = do
  guard $ V.length instrs >= 3
  Inc a <- return $ V.unsafeIndex instrs 0
  Dec b <- return $ V.unsafeIndex instrs 1
  Jnz (Reg b') (Const (-2)) <- return $ V.unsafeIndex instrs 2
  guard $ b == b'
  Just (a, b)

evalNextInstr :: Simulator -> Simulator
evalNextInstr sim@(Sim{_currentLine=cl}) =
    let (sim', i) = eval $ V.unsafeDrop cl $ sim ^. instructions
    in sim' & currentLine +~ i
    where value :: Value -> Int
          value = either' (\r -> sim ^. reg r) id
          eval :: Vector Instruction -> (Simulator, Int)
          eval (multiplication -> Just (a, b, c, d)) =
              ( sim & reg c +~ (value a * sim ^. reg b)
                    & reg b .~ 0
                    & reg d .~ 0
              , 6 )
          eval (plusEquals -> Just (a, b)) =
              ( sim & reg a +~ sim ^. (reg b)
                    & reg b .~ 0
              , 3 )
          eval instrs =
              case V.unsafeHead instrs of
                (Cpy v v') -> ( sim & either' (\r -> reg r .~ value v) (const id) v'
                              , 1 )
                (Inc r)    -> ( sim & reg r +~ 1
                              , 1 )
                (Dec r)    -> ( sim & reg r -~ 1
                              , 1 )
                (Tgl r)    -> ( sim & over (instructions . ix (sim ^. reg r + cl)) tgl
                              , 1 )
                (Out v)    -> ( sim & output %~ (|> value v)
                              , 1 )
                (Jnz v v') -> ( sim
                              , if value v /= 0 then value v' else 1 )
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
