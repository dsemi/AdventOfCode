{-# LANGUAGE ScopedTypeVariables #-}

module Utils where

import Control.Monad.ST
import Data.Either (rights)
import Data.List (tails)
import Data.STRef
import Text.Megaparsec (ParseError, Parsec, Token, many, parse, try, (<|>))
import Text.Megaparsec.Char (anyChar)
import Text.Megaparsec.Char.Lexer (decimal, signed)


findAllInts :: (Integral a) => String -> Either (ParseError (Token String) ()) [a]
findAllInts = ((map fromInteger) <$>) . parse parser ""
    where parser :: Parsec () String [Integer]
          parser = many $ try $ searchAll $ signed (return ()) decimal

searchAll :: Parsec () String a -> Parsec () String a
searchAll p = let parser = try p <|> (anyChar *> parser) in parser

findAll :: Parsec () String a -> String -> [a]
findAll parser = rights . map (parse parser "") . init . tails

swapSTRef :: STRef s a -> a -> ST s a
swapSTRef ref x = do
  v <- readSTRef ref
  writeSTRef ref x
  pure v
