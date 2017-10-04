{-# LANGUAGE StrictData, TemplateHaskell, TupleSections, ViewPatterns #-}

module Year2016.Assembunny
    ( Simulator(..)
    , a, b, c, d, output, instructions
    , evaluate
    , evaluateUntilOutputLengthIs
    , parseInstructions
    ) where

import Utils

import Control.Lens (assign, ix, over, set, use, (%=), (.=), (+=), (-=))
import Control.Lens.TH (makeLenses)
import Control.Monad
import Control.Monad.State.Strict
import Data.Foldable()
import Data.Maybe (fromJust, mapMaybe)
import Data.Sequence (Seq, empty, (|>))
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Exts
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
                     }

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

value :: Value -> State Simulator Int
value (Const i) = return i
value (Reg   r) = use $ reg r

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

eval :: Vector Instruction -> State Simulator (Int -> Int)
eval (multiplication -> Just (a, b, c, d)) = do
  a' <- value a
  b' <- use $ reg b
  reg c += a' * b'
  reg b .= 0
  reg d .= 0
  return (+6)
eval instrs =
    case V.unsafeHead instrs of
      (Cpy v v') -> do
        case v' of
          (Reg r)   -> value v >>= assign (reg r)
          (Const _) -> return ()
        return (+1)
      (Inc r) -> do
        reg r += 1
        return (+1)
      (Dec r) -> do
        reg r -= 1
        return (+1)
      (Tgl r) -> do
        i <- use $ reg r
        instrs <- use instructions
        cl <- use currentLine
        when (i + cl < V.length instrs) $ do
          modify' $ over (instructions . ix (i+cl)) tgl
        return (+1)
      (Out r) -> do
        v <- use $ reg r
        output %= (|> v)
        return (+1)
      (Jnz v v') -> do
        l <- value v'
        rv <- value v
        return $ if rv /= 0 then (+l) else (+1)
    where tgl (Cpy v v') = Jnz v v'
          tgl (Inc r)    = Dec r
          tgl (Dec r)    = Inc r
          tgl (Jnz v v') = Cpy v v'
          tgl (Tgl r)    = Inc r


evaluateWhile :: (Simulator -> Bool) -> State Simulator ()
evaluateWhile f = do
  sim <- get
  let instrs = _instructions sim
      line = _currentLine sim
      cond = f sim
  when cond $ do
    cl <- eval $ V.drop line instrs
    currentLine %= cl
    evaluateWhile f

lineInBounds :: Simulator -> Bool
lineInBounds (Sim {_currentLine=line, _instructions=instrs}) =
    line >= 0 && line < V.length instrs

outLengthIs :: Int -> Simulator -> Bool
outLengthIs n (Sim {_output=out}) = length out <= n

evaluate :: Simulator -> Simulator
evaluate = execState (evaluateWhile lineInBounds)

evaluateUntilOutputLengthIs :: Int -> Simulator -> Simulator
evaluateUntilOutputLengthIs n = execState (evaluateWhile ((&&) <$> outLengthIs n <*> lineInBounds))
