module Utils where

import Text.Megaparsec
import Text.Megaparsec.Lexer
import Text.Megaparsec.String

findAllInts :: (Integral a) => String -> Either (ParseError (Token String) Dec) [a]
findAllInts = ((map fromInteger) <$>) . parse parser ""
    where parser :: Parser [Integer]
          parser = many $ try $ searchAll $ signed (return ()) integer


searchAll :: Parser a -> Parser a
searchAll p = let parser = try p <|> (anyChar *> parser) in parser
