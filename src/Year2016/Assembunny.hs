{-# LANGUAGE StrictData, TemplateHaskell #-}

module Year2016.Assembunny
    ( Simulator(..)
    , a, b, c, d
    , evaluate
    , evaluateOutput
    , parseInstructions
    ) where

import Control.Lens
import Data.List (tails)
import Data.Maybe
import Data.Vector (Vector)
import qualified Data.Vector as V
import Text.Megaparsec
import Text.Megaparsec.Char (oneOf, space, spaceChar, string)
import Text.Megaparsec.Char.Lexer (decimal, signed)


type Register = Char
type Value = Either Register Int

data Instruction = Cpy Value Value
                 | Inc Char
                 | Dec Char
                 | Tgl Char
                 | Out Value
                 | Jnz Value Value
                 | Add Char Char
                 | Mul Value Char Char Char
                 | Nop
                   deriving (Eq, Show)

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
multiplication ( (Cpy a' (Left d'))
               : (Inc c' )
               : (Dec ((== d') -> True))
               : (Jnz (Left ((== d') -> True)) (Right (-2)))
               : (Dec b')
               : (Jnz (Left ((== b') -> True)) (Right (-5)))
               : _) =
    Just $ [ (Mul a' b' c' d')
           , Nop, Nop, Nop, Nop, Nop ]
multiplication _ = Nothing

plusEquals :: Optimization
plusEquals ( (Inc a')
           : (Dec b')
           : (Jnz (Left ((== b') -> True)) (Right (-2)))
           : _) =
    Just $  [ (Add a' b')
            , Nop, Nop ]
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
                    . map (fromJust . parseMaybe parseInstruction) . lines
    where parseInstruction :: Parsec () String Instruction
          parseInstruction = choice [ parseCpy
                                    , parseInc
                                    , parseDec
                                    , parseTgl
                                    , parseOut
                                    , parseJnz ]
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

evalInstr :: Simulator -> Instruction -> Either Int Simulator
evalInstr sim instr = eval sim
    where cl = sim ^. line
          value = val sim
          eval = case instr of
                   Cpy x y     -> Right . either (\r -> reg r .~ value x) (const id) y
                   Inc r       -> Right . (reg r +~ 1)
                   Dec r       -> Right . (reg r -~ 1)
                   Tgl r       -> Right . ((instrs . ix (value (Left r) + cl)) %~ tgl)
                   Out v       -> Left . flip val v
                   Jnz x y     -> Right . if value x /= 0 then line +~ value y - 1 else id
                   Add x y     -> Right . (reg x +~ value (Left y)) . (reg y .~ 0)
                   Mul w x y z -> Right . (reg y +~ (value w * value (Left x)))
                                  . (reg x .~ 0) . (reg z .~ 0)
                   Nop         -> Right
          tgl (Cpy x y) = Jnz x y
          tgl (Inc r)   = Dec r
          tgl (Dec r)   = Inc r
          tgl (Jnz x y) = Cpy x y
          tgl (Tgl r)   = Inc r
          tgl _ = error "Invalid toggle"

evaluate' :: Simulator -> ([Int], Simulator)
evaluate' sim = maybe ([], sim) f $ sim ^? (instrs . ix (sim ^. line))
    where f instr = case evalInstr sim instr of
                      Right sim' -> evaluate' (line +~ 1 $ sim')
                      Left v -> _1 %~ (v :) $ evaluate' (line +~ 1 $ sim)

evaluate :: Simulator -> Simulator
evaluate = snd . evaluate'

evaluateOutput :: Simulator -> [Int]
evaluateOutput = fst . evaluate'
