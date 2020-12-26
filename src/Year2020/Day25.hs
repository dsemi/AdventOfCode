{-# LANGUAGE BangPatterns, DataKinds, ScopedTypeVariables #-}

module Year2020.Day25
    ( part1
    , part2
    ) where

import Utils

import GHC.TypeLits (KnownNat)
import qualified Data.Map.Strict as M
import Data.Mod
import Math.NumberTheory.Powers
-- import Math.NumberTheory.Moduli.Class (isMultElement, powMod)
-- import qualified Math.NumberTheory.Moduli.Class as MC
-- import Math.NumberTheory.Moduli.DiscreteLogarithm
-- import Math.NumberTheory.Moduli.PrimitiveRoot
-- import Math.NumberTheory.Moduli.Singleton


part1 :: String -> Mod 20201227
part1 input = go 7 card door
    where [card, door] = findAllInts input
          go !n !cardn !doorn
              | n == card = doorn
              | n == door = cardn
              | otherwise = go (n * 7) (cardn * card) (doorn * door)

discreteLog :: forall a. (KnownNat a) => Mod a -> Mod a -> Mod a
discreteLog g h = go 0 h
    where m = fromInteger $ integerSquareRoot $ toInteger $ unMod (maxBound :: Mod a)
          tbl = M.fromList $ zip (iterate (*g) 1) [0..m]
          factor = g ^% unMod (maxBound - m)
          go i n
              | i > m = error "wat"
              | M.member n tbl = i * m + tbl M.! n
              | otherwise = go (i+1) $ n * factor


part2 :: String -> Mod 20201227
part2 input = card ^% unMod (discreteLog 7 door)
    where [card, door] = findAllInts input


-- part2 :: String -> Maybe (MC.Mod 20201227)
-- part2 input = fmap (powMod door) $ do
  -- cg <- cyclicGroup :: Maybe (CyclicGroup Integer 20201227)
  -- discreteLogarithm cg <$> isPrimitiveRoot cg 7 <*> isMultElement card
    -- where [card, door] = findAllInts input
