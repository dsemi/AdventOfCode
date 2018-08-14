{-# LANGUAGE ScopedTypeVariables #-}

module Utils where

import Control.Monad.ST
import Data.Either (rights)
import Data.List (tails)
import Data.STRef
import Data.Void (Void)
import Text.Megaparsec (ParseError, Parsec, Token, many, parse, try, (<|>))
import Text.Megaparsec.Char (anyChar)
import Text.Megaparsec.Char.Lexer (decimal, signed)


type Parser = Parsec Void String

findAllInts :: (Integral a) => String -> Either (ParseError (Token String) Void) [a]
findAllInts = ((map fromInteger) <$>) . parse parser ""
    where parser :: Parser [Integer]
          parser = many $ try $ searchAll $ signed (return ()) decimal

searchAll :: Parser a -> Parser a
searchAll p = let parser = try p <|> (anyChar *> parser) in parser

findAll :: Parser a -> String -> [a]
findAll parser = rights . map (parse parser "") . init . tails

swapSTRef :: STRef s a -> a -> ST s a
swapSTRef ref x = do
  v <- readSTRef ref
  writeSTRef ref x
  pure v
