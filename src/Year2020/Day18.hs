module Year2020.Day18
    ( part1
    , part2
    ) where

import Control.Monad.Combinators.Expr
import Data.Maybe
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L


sc :: Parsec () String ()
sc = L.space space1 empty empty

add :: Operator (Parsec () String) Int
add = InfixL ((+) <$ L.symbol sc "+")

mul :: Operator (Parsec () String) Int
mul = InfixL ((*) <$ L.symbol sc "*")

parseExprs :: [[Operator (Parsec () String) Int]] -> String -> [Int]
parseExprs ops = map (fromJust . parseMaybe pExpr) . lines
    where pExpr = makeExprParser pTerm ops
          pTerm = choice [ between (L.symbol sc "(") (L.symbol sc ")") pExpr
                         , L.lexeme sc L.decimal ]

part1 :: String -> Int
part1 = sum . parseExprs [[add, mul]]

part2 :: String -> Int
part2 = sum . parseExprs [[add], [mul]]
