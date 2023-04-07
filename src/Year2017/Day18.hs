module Year2017.Day18
    ( part1
    , part2
    ) where

import Control.Applicative (asum)
import Data.Array.Unboxed
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Vector (Vector)
import qualified Data.Vector as V
import FlatParse.Basic

import Utils

type Value = Either Char Int
data Instr = Snd Value
           | Set Char Value
           | Add Char Value
           | Mul Char Value
           | Mod Char Value
           | Rcv Char
           | Jgz Value Value

parseInstrs :: ByteString -> Vector Instr
parseInstrs = V.fromList . map parse . B.lines
    where parse ln = case runParser instr ln of
                       OK res _ -> res
                       _ -> error "unreachable"
          instr = asum [ $(string "snd ") >> Snd <$> value
                       , $(string "set ") >> Set <$> letterChar <* $(char ' ') <*> value
                       , $(string "add ") >> Add <$> letterChar <* $(char ' ') <*> value
                       , $(string "mul ") >> Mul <$> letterChar <* $(char ' ') <*> value
                       , $(string "mod ") >> Mod <$> letterChar <* $(char ' ') <*> value
                       , $(string "rcv ") >> Rcv <$> letterChar
                       , $(string "jgz ") >> Jgz <$> value <* $(char ' ') <*> value
                       ]
          letterChar = satisfy isLatinLetter
          value = (Left <$> letterChar) <|> (Right <$> signedInt)

data Sig = Send Int | Recv Int deriving (Eq)

isRecv :: Sig -> Bool
isRecv (Recv _) = True
isRecv _ = False

couple :: [Sig] -> [Sig]
couple [] = []
couple (x:xs) = if isRecv x then xs else x : couple xs

run :: (UArray Char Int -> UArray Char Int) -> Vector Instr -> [Sig] -> [Sig]
run f instrs = go 0 $ f $ listArray ('a', 'z') $ repeat 0
    where go :: Int -> UArray Char Int -> [Sig] -> [Sig]
          go line regs ins =
              case instrs V.! line of
                (Snd v) -> Send (value v) : step 1 (couple ins) regs
                (Set r v) -> step 1 ins $ set r (value v)
                (Add r v) -> step 1 ins $ set r (get r + value v)
                (Mul r v) -> step 1 ins $ set r (get r * value v)
                (Mod r v) -> step 1 ins $ set r (get r `mod` value v)
                (Rcv r) -> Recv (get r) : case ins of
                                            Send v : rest -> step 1 rest $ set r v
                                            _ -> []
                (Jgz a b) -> step (if value a > 0 then value b else 1) ins regs
              where get r = regs ! r
                    set r v = regs // [(r, v)]
                    value = either get id
                    step n i r = go (line + n) r i

part1 :: ByteString -> Int
part1 input = go 0 sigs
    where sigs = run id (parseInstrs input) sigs
          go _ (Send x : xs) = go x xs
          go v (Recv 0 : xs) = go v xs
          go v _ = v

part2 :: ByteString -> Int
part2 input = length $ filter (not . isRecv) p1
    where instrs = parseInstrs input
          p0 = run id instrs p1
          p1 = run (// [('p', 1)]) instrs p0
