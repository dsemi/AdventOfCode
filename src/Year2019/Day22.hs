{-# LANGUAGE DataKinds, ScopedTypeVariables #-}

module Year2019.Day22
    ( part1
    , part2
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Group
import Data.Maybe
import Data.Mod
import FlatParse.Basic
import GHC.TypeLits (KnownNat)

import Utils

data LinearTrans n = LinearTrans (Mod n) (Mod n)

parseTechs :: forall n. (KnownNat n) => ByteString -> LinearTrans n
parseTechs = mconcat . map parse . B.lines
    where sint = fromIntegral <$> signedInt
          tech = (LinearTrans (-1) maxBound <$ $(string "deal into new stack"))
                 <|> ((\n -> LinearTrans 1 (- n)) <$> ($(string "cut ") *> sint))
                 <|> ((\n -> LinearTrans n 0) <$> ($(string "deal with increment ") *> sint))
          parse line = case runParser tech line of
                         OK res _ -> res
                         _ -> error "unreachable"

instance (KnownNat n) => Semigroup (LinearTrans n) where
    LinearTrans a b <> LinearTrans a' b' = LinearTrans (a' * a) (a' * b + b')

instance (KnownNat n) => Monoid (LinearTrans n) where
    mempty = LinearTrans 1 0

instance forall n. (KnownNat n) => Group (LinearTrans n) where
    invert (LinearTrans a b) = LinearTrans a' b'
        where a' = fromJust $ invertMod a
              b' = negate $ a' * b

shuffle :: (KnownNat n) => Int -> Mod n -> LinearTrans n -> Mod n
shuffle n i t = a*i + b
    where LinearTrans a b = t `pow` n

part1 :: ByteString -> Mod 10007
part1 = shuffle 1 2019 . parseTechs

part2 :: ByteString -> Mod 119315717514047
part2 = shuffle (-101741582076661) 2020 . parseTechs
