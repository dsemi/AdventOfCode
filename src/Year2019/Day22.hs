{-# LANGUAGE DataKinds, ScopedTypeVariables, TypeApplications #-}

module Year2019.Day22
    ( part1
    , part2
    ) where

import Data.Group
import Data.Maybe
import Data.Mod
import GHC.TypeLits (KnownNat)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer


data LinearTrans n = LinearTrans (Mod n) (Mod n)

parseTechs :: forall n. (KnownNat n) => String -> LinearTrans n
parseTechs = mconcat . map (fromJust . parseMaybe tech) . lines
    where sint = fromInteger <$> signed (pure ()) decimal
          tech :: Parsec () String (LinearTrans n)
          tech = LinearTrans (-1) maxBound <$ string "deal into new stack"
                 <|> (\n -> LinearTrans 1 (- n)) <$> (string "cut " *> sint)
                 <|> (\n -> LinearTrans n 0) <$> (string "deal with increment " *> sint)

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

part1 :: String -> Mod 10007
part1 = shuffle 1 2019 . parseTechs

part2 :: String -> Mod 119315717514047
part2 = shuffle (-101741582076661) 2020 . parseTechs
