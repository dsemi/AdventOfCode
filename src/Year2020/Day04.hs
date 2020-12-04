module Year2020.Day04
    ( part1
    , part2
    ) where


import Control.Monad
import Data.Char
import Data.List.Split (splitOn)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Maybe
import Text.Megaparsec
import Text.Megaparsec.Char (char, hexDigitChar, lowerChar, space)
import Text.Megaparsec.Char.Lexer (decimal)


validPassports :: Bool -> String -> Int
validPassports validateFields = length . mapMaybe (parseMaybe (passport S.empty)) . splitOn "\n\n"
    where reqFields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
          passport :: Set String -> Parsec () String ()
          passport s = try (do
            field <- some lowerChar <* char ':'
            if validateFields then
                case field of
                  "byr" -> decimal >>= \n -> guard $ 1920 <= n && n <= 2002
                  "iyr" -> decimal >>= \n -> guard $ 2010 <= n && n <= 2020
                  "eyr" -> decimal >>= \n -> guard $ 2020 <= n && n <= 2030
                  "hgt" -> do h <- decimal
                              u <- takeP Nothing 2
                              guard $ u == "cm" && 150 <= h && h <= 193
                                      || u == "in" && 59 <= h && h <= 76
                  "hcl" -> void $ char '#' >> count 6 hexDigitChar
                  "ecl" -> void $ choice $ map chunk [ "amb", "blu", "brn", "gry"
                                                     , "grn", "hzl", "oth"]
                  "pid" -> void $ count 9 (satisfy isDigit)
                  _ -> void $ some $ satisfy $ not . isSpace
            else void $ some $ satisfy $ not . isSpace
            space
            passport (S.insert field s)) <|> guard (all (`S.member` s) reqFields)

part1 :: String -> Int
part1 = validPassports False

part2 :: String -> Int
part2 = validPassports True
