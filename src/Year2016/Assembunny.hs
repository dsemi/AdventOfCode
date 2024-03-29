{-# LANGUAGE RankNTypes, StrictData, TemplateHaskell #-}

module Year2016.Assembunny
    ( Simulator(..)
    , a, b, c, d
    , evaluate
    , evaluateOutput
    , parseInstructions
    ) where

import Control.Applicative (asum)
import Control.Lens
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.List (tails)
import Data.Maybe
import Data.Vector (Vector)
import qualified Data.Vector as V
import FlatParse.Basic hiding (take)

import Utils

type Value = Either Char Int

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

reg :: Functor f => Char -> (Int -> f Int) -> Simulator -> f Simulator
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

parseInstructions :: ByteString -> Simulator
parseInstructions = Sim 0 0 0 0 0
                    . V.fromList . optimize [multiplication, plusEquals]
                    . map parse . B.lines
    where parse ln = case runParser parseInstruction ln of
                       OK res _ -> res
                       _ -> error "unreachable"
          parseInstruction =
              asum [ $(string "cpy ") >> Cpy <$> value <* $(char ' ') <*> value
                   , $(string "inc ") >> Inc <$> register
                   , $(string "dec ") >> Dec <$> register
                   , $(string "tgl ") >> Tgl <$> register
                   , $(string "out ") >> Out <$> value
                   , $(string "jnz ") >> Jnz <$> value <* $(char ' ') <*> value ]
          register = satisfy (`elem` "abcd")
          value = (Left <$> register) <|> (Right <$> signedInt)

val :: Simulator -> Value -> Int
val sim = either ((sim ^.) . reg) id

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
