module Year2020.Day18
    ( part1
    , part2
    ) where

import Control.Monad.Combinators.Expr
import Data.Maybe
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L


data Expr = Const Int
          | Add Expr Expr
          | Mul Expr Expr

sc :: Parsec () String ()
sc = L.space space1 empty empty

add :: Operator (Parsec () String) Expr
add = InfixL (Add <$ L.symbol sc "+")

mul :: Operator (Parsec () String) Expr
mul = InfixL (Mul <$ L.symbol sc "*")

parseExprs :: [[Operator (Parsec () String) Expr]] -> String -> [Expr]
parseExprs ops = map (fromJust . parseMaybe pExpr) . lines
    where pExpr = makeExprParser pTerm ops
          pTerm = choice [ between (L.symbol sc "(") (L.symbol sc ")") pExpr
                         , Const <$> L.lexeme sc L.decimal ]

eval :: Expr -> Int
eval (Const n) = n
eval (Add a b) = eval a + eval b
eval (Mul a b) = eval a * eval b

part1 :: String -> Int
part1 = sum . map eval . parseExprs [[add, mul]]

part2 :: String -> Int
part2 = sum . map eval . parseExprs [[add], [mul]]
