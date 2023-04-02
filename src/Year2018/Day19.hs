module Year2018.Day19
    ( part1
    , part2
    , Op(..)
    , Instr(..)
    , parseInstrs
    , eval
    ) where

import qualified Control.Applicative as A
import Data.Bits
import Data.Bool
import Data.ByteString (ByteString)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Vector.Unboxed ((!), (//))
import qualified Data.Vector.Unboxed as U
import Math.NumberTheory.ArithmeticFunctions
import FlatParse.Basic


data Op = Addr | Addi | Mulr | Muli | Banr | Bani | Borr | Bori
        | Setr | Seti | Gtir | Gtri | Gtrr | Eqir | Eqri | Eqrr
          deriving (Show)

data Instr = Instr Op Int Int Int deriving (Show)

parseInstrs :: ByteString -> (Int, Vector Instr)
parseInstrs input = case runParser parser input of
                      OK res _ -> res
                      _ -> error "unreachable"
    where parser = do
            ip <- $(string "#ip ") *> anyAsciiDecimalInt
            instrs <- V.fromList <$> some ($(char '\n') *> instr)
            pure (ip, instrs)
          instr = do
            op <- A.asum [ $(string "addr") *> pure Addr
                         , $(string "addi") *> pure Addi
                         , $(string "mulr") *> pure Mulr
                         , $(string "muli") *> pure Muli
                         , $(string "banr") *> pure Banr
                         , $(string "bani") *> pure Bani
                         , $(string "borr") *> pure Borr
                         , $(string "bori") *> pure Bori
                         , $(string "setr") *> pure Setr
                         , $(string "seti") *> pure Seti
                         , $(string "gtir") *> pure Gtir
                         , $(string "gtri") *> pure Gtri
                         , $(string "gtrr") *> pure Gtrr
                         , $(string "eqir") *> pure Eqir
                         , $(string "eqri") *> pure Eqri
                         , $(string "eqrr") *> pure Eqrr ]
            Instr op <$> ($(char ' ') *> anyAsciiDecimalInt) <*>
                         ($(char ' ') *> anyAsciiDecimalInt) <*>
                         ($(char ' ') *> anyAsciiDecimalInt)

eval :: Bool -> U.Vector Int -> Instr -> (Maybe Int, U.Vector Int)
eval d21 v (Instr op a b c) =
    case op of
      Addr -> (Nothing,) $ v // [(c, v ! a + v ! b)]
      Addi -> (Nothing,) $ v // [(c, v ! a + b)]
      Mulr -> (Nothing,) $ v // [(c, v ! a * v ! b)]
      Muli -> (Nothing,) $ v // [(c, v ! a * b)]
      Banr -> (Nothing,) $ v // [(c, v ! a .&. v ! b)]
      Bani -> (Nothing,) $ v // [(c, v ! a .&. b)]
      Borr -> (Nothing,) $ v // [(c, v ! a .|. v ! b)]
      Bori -> (Nothing,) $ v // [(c, v ! a .|. b)]
      Setr -> (Nothing,) $ v // [(c, v ! a)]
      Seti -> (Nothing,) $ v // [(c, a)]
      Gtir -> (Nothing,) $ v // [(c, bool 0 1 $ a > v ! b)]
      Gtri -> (Nothing,) $ v // [(c, bool 0 1 $ v ! a > b)]
      Gtrr -> (Nothing,) $ v // [(c, bool 0 1 $ v ! a > v ! b)]
      Eqir -> (Nothing,) $ v // [(c, bool 0 1 $ a == v ! b)]
      Eqri -> (Nothing,) $ v // [(c, bool 0 1 $ v ! a == b)]
      Eqrr -> let v' = v // [(c, bool 0 1 $ v ! a == v ! b)]
              in if d21 then (Just $ v' ! a, v')
                 else (Just $ v' ! b, v')

run :: U.Vector Int -> Int -> Vector Instr -> Maybe Int
run registers ip instrs = go 0 registers
    where go i regs
              | i < 0 || i >= V.length instrs = Nothing
              | otherwise = let (v, regs') = eval False (regs // [(ip, i)]) (instrs V.! i)
                            in v A.<|> go (regs' ! ip + 1) regs'

part1 :: ByteString -> Maybe Int
part1 = fmap (sigma 1) . uncurry (run $ U.replicate 6 0) . parseInstrs

-- Not sure if there's a better way than just deconstructing the assembly
part2 :: ByteString -> Maybe Int
part2 = fmap (sigma 1) . uncurry (run $ U.cons 1 $ U.replicate 5 0) . parseInstrs
