{-# LANGUAGE OverloadedStrings #-}

module Year2020.Day04
    ( part1
    , part2
    ) where

import Control.Applicative hiding ((<|>))
import Control.Monad
import Data.ByteString (ByteString)
import Data.Char (isHexDigit, isLower, isSpace)
import qualified Data.HashSet as S
import FlatParse.Basic as F

import Utils

validPassports :: Bool -> ByteString -> Int
validPassports validateFields = length . filter parse . splitOn "\n\n"
    where reqFields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
          passport s = try (do
            field <- byteStringOf (some $ satisfy isLower) <* $(char ':')
            if validateFields then
                case field of
                  "byr" -> anyAsciiDecimalInt >>= \n -> guard $ 1920 <= n && n <= 2002
                  "iyr" -> anyAsciiDecimalInt >>= \n -> guard $ 2010 <= n && n <= 2020
                  "eyr" -> anyAsciiDecimalInt >>= \n -> guard $ 2020 <= n && n <= 2030
                  "hgt" -> do h <- anyAsciiDecimalInt
                              u <- F.take 2
                              guard $ u == "cm" && 150 <= h && h <= 193
                                      || u == "in" && 59 <= h && h <= 76
                  "hcl" -> $(char '#') >> replicateM_ 6 (satisfy isHexDigit)
                  "ecl" -> asum $ [ $(string "amb"), $(string "blu"), $(string "brn"), $(string "gry")
                                  , $(string "grn"), $(string "hzl"), $(string "oth") ]
                  "pid" -> replicateM_ 9 (satisfy isDigit)
                  _ -> skipSome $ satisfy $ not . isSpace
            else skipSome $ satisfy $ not . isSpace
            skipMany $ satisfy isSpace
            passport (S.insert field s)) <|> guard (all (`S.member` s) reqFields)
          parse pass = case runParser (passport S.empty) pass of
                         OK _ _ -> True
                         _ -> False

part1 :: ByteString -> Int
part1 = validPassports False

part2 :: ByteString -> Int
part2 = validPassports True
