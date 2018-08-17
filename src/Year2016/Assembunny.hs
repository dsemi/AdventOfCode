{-# LANGUAGE StrictData, TemplateHaskell, ViewPatterns #-}

module Year2016.Assembunny
    ( Simulator(..)
    , a, b, c, d
    , evaluate
    , evaluateOutput
    , parseInstructions
    ) where

import Utils

import Control.Lens
import Data.List (tails)
import Data.Maybe
import Data.Vector (Vector)
import qualified Data.Vector as V
import Text.Megaparsec (choice, eitherP, parseMaybe)
import Text.Megaparsec.Char (oneOf, space, spaceChar, string)
import Text.Megaparsec.Char.Lexer (decimal, signed)


type Register = Char
type Value = Either Register Int

data Instr = Cpy Value Value
           | Inc Char
           | Dec Char
           | Tgl Char
           | Jnz Value Value
           | Add Char Char
           | Mul Value Char Char Char
           | Nop deriving (Eq, Show)

data Action = Out Value deriving (Eq, Show)

type Instruction = Either Action Instr

data Simulator = Sim { _a :: Int
                     , _b :: Int
                     , _c :: Int
                     , _d :: Int
                     , _line :: Int
                     , _instrs :: Vector Instruction
                     }
makeLenses ''Simulator

reg :: Applicative f => Register -> (Int -> f Int) -> Simulator -> f Simulator
reg 'a' = a
reg 'b' = b
reg 'c' = c
reg 'd' = d
reg _ = error "Invalid register"

type Optimization = [Instruction] -> Maybe [Instruction]

multiplication :: Optimization
multiplication ( Right (Cpy a' (Left d'))
               : Right (Inc c' )
               : Right (Dec ((== d') -> True))
               : Right (Jnz (Left ((== d') -> True)) (Right (-2)))
               : Right (Dec b')
               : Right (Jnz (Left ((== b') -> True)) (Right (-5)))
               : _) =
    Just [ Right (Mul a' b' c' d')
         , Right Nop, Right Nop, Right Nop, Right Nop, Right Nop ]
multiplication _ = Nothing

plusEquals :: Optimization
plusEquals ( Right (Inc a')
           : Right (Dec b')
           : Right (Jnz (Left ((== b') -> True)) (Right (-2)))
           : _) =
    Just [ Right (Add a' b')
         , Right Nop, Right Nop ]
plusEquals _ = Nothing

optimize :: [Optimization] -> [Instruction] -> [Instruction]
optimize [] v = v
optimize (f:fs) v = optimize fs $ collapse [] opt
    where opt = map (\x -> fromMaybe (take 1 x) $ f x) $ init $ tails v
          collapse [] [] = []
          collapse [] ((i:is):xs) = i : collapse is xs
          collapse (i:is) (_:xs) = i : collapse is xs
          collapse _ _ = error "Bad collapse"

parseInstructions :: String -> Simulator
parseInstructions = Sim 0 0 0 0 0
                    . V.fromList . optimize [multiplication, plusEquals]
                    . map (fromJust . parseMaybe (eitherP parseAction parseInstruction)) . lines
    where parseInstruction :: Parser Instr
          parseInstruction = choice [ parseCpy
                                    , parseInc
                                    , parseDec
                                    , parseTgl
                                    , parseJnz ]
          parseAction :: Parser Action
          parseAction = parseOut
          int = signed space decimal
          register = oneOf "abcd"
          value = eitherP register int
          parseCpy = string "cpy " >> Cpy <$> value <* spaceChar <*> value
          parseInc = string "inc " >> Inc <$> register
          parseDec = string "dec " >> Dec <$> register
          parseTgl = string "tgl " >> Tgl <$> register
          parseOut = string "out " >> Out <$> value
          parseJnz = string "jnz " >> Jnz <$> value <* spaceChar <*> value

val :: Simulator -> Value -> Int
val sim = either ((sim ^?!) . reg) id

evalInstr :: Simulator -> Instr -> Simulator
evalInstr sim instr = eval sim
    where cl = sim ^. line
          value = val sim
          eval = case instr of
                   (Cpy x y)     -> either (\r -> reg r .~ value x) (const id) y
                   (Inc r)       -> reg r +~ 1
                   (Dec r)       -> reg r -~ 1
                   (Tgl r)       -> (instrs . ix (value (Left r) + cl) . _Right) %~ tgl
                   (Jnz x y)     -> if value x /= 0 then line +~ value y - 1 else id
                   (Add x y)     -> (reg x +~ value (Left y)) . (reg y .~ 0)
                   (Mul w x y z) -> (reg y +~ (value w * value (Left x)))
                                    . (reg x .~ 0) . (reg z .~ 0)
                   Nop           -> id
          tgl (Cpy x y) = Jnz x y
          tgl (Inc r)   = Dec r
          tgl (Dec r)   = Inc r
          tgl (Jnz x y) = Cpy x y
          tgl (Tgl r)   = Inc r
          tgl _ = error "Invalid toggle"

evalAction :: Simulator -> Action -> Int
evalAction sim (Out v) = val sim v

evaluate :: Simulator -> Simulator
evaluate sim = maybe sim f $ sim ^? (instrs . ix (sim ^. line))
    where f = evaluate . (line +~ 1) . either (error "") (evalInstr sim)

evaluateOutput :: Simulator -> [Int]
evaluateOutput sim = maybe [] f $ sim ^? (instrs . ix (sim ^. line))
    where f = \case
              Right instr -> evaluateOutput (line +~ 1 $ evalInstr sim instr)
              Left action -> evalAction sim action : evaluateOutput (line +~ 1 $ sim)
