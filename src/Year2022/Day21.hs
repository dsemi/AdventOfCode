module Year2022.Day21
    ( part1
    , part2
    ) where

import Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as M
import Data.Maybe
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

data Op = Add | Sub | Mul | Div
data Monkey = Num Int | Math String Op String

eval :: Op -> Int -> Int -> Int
eval Add = (+)
eval Sub = (-)
eval Mul = (*)
eval Div = div

monkeys :: String -> HashMap String Monkey
monkeys = M.fromList . map (fromJust . parseMaybe @() monkey) . lines
    where name = some letterChar
          monkey = (,) <$> name <* chunk ": " <*> (num <|> math)
          num = Num <$> decimal
          math = Math <$> name <* spaceChar <*> (op <$> oneOf "+-*/") <* spaceChar <*> name
          op '+' = Add
          op '-' = Sub
          op '*' = Mul
          op '/' = Div
          op _ = error "Malformed input"

part1 :: String -> Int
part1 input = val "root"
    where ms = monkeys input
          val k = case ms ! k of
                    Num n -> n
                    Math l op r -> eval op (val l) (val r)

part2 :: String -> Int
part2 input = let (Math l _ r) = ms ! "root"
              in case (val l, val r) of
                   (Left f, Right n) -> f n
                   (Right n, Left f) -> f n
                   _ -> error "Not possible"
    where ms = monkeys input
          val "humn" = Left id
          val k = case ms ! k of
                    Num n -> Right n
                    Math l op r ->
                        let left = val l
                            right = val r
                        in case (left, right, op) of
                             (Left f, Right n, Add) -> Left $ f . subtract n
                             (Left f, Right n, Sub) -> Left $ f . (+ n)
                             (Left f, Right n, Mul) -> Left $ f . (`div` n)
                             (Left f, Right n, Div) -> Left $ f . (* n)
                             (Right n, Left f, Add) -> Left $ f . subtract n
                             (Right n, Left f, Sub) -> Left $ f . (n -)
                             (Right n, Left f, Mul) -> Left $ f . (`div` n)
                             (Right n, Left f, Div) -> Left $ f . div n
                             (Right a, Right b, _) -> Right $ eval op a b
                             _ -> error "Not possible"
