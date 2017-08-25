{-# LANGUAGE StrictData, TemplateHaskell, TupleSections, ViewPatterns #-}

module Year2016.Assembunny
    ( Simulator(..)
    , a, b, c, d, output, instructions
    , evaluate
    , evaluateUntilOutputLengthIs
    , parseInstructions
    ) where

import Control.Lens ((%=), (.=), (+=), (-=), assign, ix, over, set, use)
import Control.Lens.TH (makeLenses)
import Control.Monad
import Control.Monad.State.Strict
import Data.Data
import Data.Foldable()
import Data.Maybe (fromJust, mapMaybe)
import Data.Sequence (Seq, (|>), empty)
import Data.Vector ((!), Vector)
import qualified Data.Vector as V
import Data.Vector.Lens (sliced)
import Text.Megaparsec ((<|>), eitherP, oneOf, parseMaybe, space, spaceChar, string)
import Text.Megaparsec.Lexer (integer, signed)
import Text.Megaparsec.String (Parser)


data Value = Reg Char | Const Int deriving (Eq, Show)

data Instruction = Cpy Value Value
                 | Inc Char
                 | Dec Char
                 | Tgl Char
                 | Out Char
                 | Jnz Value Value
                 | Mul Value Char Char Char deriving (Eq, Show)

data Simulator = Sim { _a :: Int
                     , _b :: Int
                     , _c :: Int
                     , _d :: Int
                     , _currentLine :: Int
                     , _outputLim :: Int
                     , _output :: Seq Int
                     , _instructions :: Vector Instruction
                     } deriving (Eq, Show)

makeLenses ''Simulator


parseInstructions :: String -> Simulator
parseInstructions = Sim 0 0 0 0 0 0 empty . V.fromList
                    . map (fromJust . parseMaybe parseInstruction) . lines
    where parseInstruction :: Parser Instruction
          parseInstruction = parseCpy
                             <|> parseInc
                             <|> parseDec
                             <|> parseTgl
                             <|> parseOut
                             <|> parseJnz
          int = signed space $ fromInteger <$> integer
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

evalInstr :: Instruction -> State Simulator ()
evalInstr (Cpy v v') =
    case v' of
      (Const _) -> return ()
      (Reg r)   -> value v >>= assign (reg r)
evalInstr (Inc r) = reg r += 1
evalInstr (Dec r) = reg r -= 1
evalInstr (Tgl r) = do
  i <- use $ reg r
  instrs <- use instructions
  cl <- use currentLine
  when (i + cl < V.length instrs) $ do
    modify' $ over (instructions . ix (i+cl)) tgl
      where tgl (Cpy v v') = Jnz v v'
            tgl (Inc r)    = Dec r
            tgl (Dec r)    = Inc r
            tgl (Jnz v v') = Cpy v v'
            tgl (Tgl r)    = Inc r
evalInstr (Out r) = do
  v <- use $ reg r
  output %= (|> v)
evalInstr (Jnz v v') = do
  l <- value v'
  rv <- value v
  when (rv /= 0) $
       currentLine += l - 1
evalInstr (Mul a b c d) = do
  a' <- value a
  b' <- use $ reg b
  reg c += a' * b'
  reg b .= 0
  reg d .= 0

optimize :: Vector Instruction -> Vector Instruction
optimize v = foldr replaceMul v
             $ mapMaybe (\i -> (i,) <$> matchesMul (V.slice i 6 v)) [0 .. V.length v - 6]
    where matchesMul (V.toList -> [ Cpy a (Reg d)
                                  , Inc c
                                  , Dec d'
                                  , Jnz (Reg d'') (Const (-2))
                                  , Dec b
                                  , Jnz (Reg b') (Const (-5))])
              | d == d' && d' == d'' && b == b' = Just (a, b, c, d)
              | otherwise = Nothing
          matchesMul _ = Nothing
          noop = Jnz (Const 0) (Const 0)
          toMul a b c d = V.fromList $ Mul a b c d : replicate 5 noop
          replaceMul (i,(a,b,c,d)) = set (sliced i 6) (toMul a b c d)

evaluateWith :: (Instruction -> State Simulator ()) -> Simulator -> Simulator
evaluateWith evalInstr = execState eval . over instructions optimize
    where eval :: State Simulator ()
          eval = do
            line <- use currentLine
            instrs <- use instructions
            when (line >= 0 && line < V.length instrs) $ do
              evalInstr $ instrs ! line
              currentLine += 1
              eval


evaluate :: Simulator -> Simulator
evaluate = evaluateWith evalInstr

evaluateUntilOutputLengthIs :: Int -> Simulator -> Simulator
evaluateUntilOutputLengthIs n = evaluateWith evalInstr'
    where evalInstr' (Out r) = do
            evalInstr $ Out r
            o <- use output
            when (length o > n) $
                 currentLine .= -100 -- Set to some value that will 'break' out of eval loop
          evalInstr' x = evalInstr x
