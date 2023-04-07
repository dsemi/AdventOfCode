{-# LANGUAGE OverloadedStrings #-}

module Year2022.Day21
    ( part1
    , part2
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Complex.Cyclotomic
import Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as M
import FlatParse.Basic

type Op = Cyclotomic -> Cyclotomic -> Cyclotomic
data Monkey = Num Cyclotomic | Math ByteString Op ByteString

monkeys :: ByteString -> HashMap ByteString Monkey
monkeys = M.fromList . map parse . B.lines
    where parse line = case runParser monkey line of
                         OK res _ -> res
                         _ -> error "unreachable"
          name = byteStringOf $ some $ satisfy isLatinLetter
          monkey = (,) <$> name <* $(string ": ") <*> (num <|> math)
          num = Num . fromIntegral <$> anyAsciiDecimalInt
          math = Math <$> name <* $(char ' ') <*> op <* $(char ' ') <*> name
          op = ($(char '+') >> pure (+)) <|>
               ($(char '-') >> pure (-)) <|>
               ($(char '*') >> pure (*)) <|>
               ($(char '/') >> pure (/))

eval :: ByteString -> HashMap ByteString Monkey -> Cyclotomic
eval k m = case m ! k of
             Num n -> n
             Math l op r -> op (eval l m) (eval r m)

part1 :: ByteString -> Cyclotomic
part1 = eval "root" . monkeys

part2 :: ByteString -> Cyclotomic
part2 input = -real n / imag n
    where n = eval "root" $ M.adjust f "root" $ M.insert "humn" (Num i) $ monkeys input
          f (Math l _ r) = Math l (-) r
          f _ = error "bad root"
