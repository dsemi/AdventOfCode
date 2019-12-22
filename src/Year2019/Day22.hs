{-# LANGUAGE DataKinds, ScopedTypeVariables, TypeApplications #-}

module Year2019.Day22
    ( part1
    , part2
    ) where

import Data.Finite
import Data.Group
import Data.Maybe
import GHC.TypeLits (KnownNat)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer


data LinearTrans n = LinearTrans (Finite n) (Finite n)

parseTechs :: (KnownNat n) => String -> LinearTrans n
parseTechs = mconcat . map (fromJust . parseMaybe tech) . lines
    where sint = signed (pure ()) decimal
          tech :: (KnownNat n) => Parsec () String (LinearTrans n)
          tech = LinearTrans (-1) maxBound <$ string "deal into new stack"
                 <|> (\n -> LinearTrans 1 (-modulo n)) <$> (string "cut " *> sint)
                 <|> (\n -> LinearTrans (modulo n) 0) <$> (string "deal with increment " *> decimal)

instance (KnownNat n) => Semigroup (LinearTrans n) where
    LinearTrans a b <> LinearTrans a' b' = LinearTrans (a' * a) (a' * b + b')

instance (KnownNat n) => Monoid (LinearTrans n) where
    mempty = LinearTrans 1 0

instance forall n. (KnownNat n) => Group (LinearTrans n) where
    invert (LinearTrans a b) = LinearTrans a' b'
        where a' = a ^ (maxBound @(Finite n) - 1)
              b' = negate $ a' * b

shuffle :: (KnownNat n) => Int -> Finite n -> LinearTrans n -> Finite n
shuffle n i t = a*i + b
    where LinearTrans a b = t `pow` n

part1 :: String -> Finite 10007
part1 = shuffle 1 2019 . parseTechs

part2 :: String -> Finite 119315717514047
part2 = shuffle (-101741582076661) 2020 . parseTechs
