module Year2022.Day21
    ( part1
    , part2
    ) where

import Data.Complex.Cyclotomic
import Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as M
import Data.Maybe
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

type Op = Cyclotomic -> Cyclotomic -> Cyclotomic
data Monkey = Num Cyclotomic | Math String Op String

monkeys :: String -> HashMap String Monkey
monkeys = M.fromList . map (fromJust . parseMaybe @() monkey) . lines
    where name = some letterChar
          monkey = (,) <$> name <* chunk ": " <*> (num <|> math)
          num = Num <$> decimal
          math = Math <$> name <* spaceChar <*> op <* spaceChar <*> name
          op = (char '+' >> pure (+)) <|>
               (char '-' >> pure (-)) <|>
               (char '*' >> pure (*)) <|>
               (char '/' >> pure (/))

eval :: String -> HashMap String Monkey -> Cyclotomic
eval k m = case m ! k of
             Num n -> n
             Math l op r -> op (eval l m) (eval r m)

part1 :: String -> Cyclotomic
part1 = eval "root" . monkeys

part2 :: String -> Cyclotomic
part2 input = -real n / imag n
    where n = eval "root" $ M.adjust f "root" $ M.insert "humn" (Num i) $ monkeys input
          f (Math l _ r) = Math l (-) r
          f _ = error "bad root"
