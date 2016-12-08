module Year2016.Day07
    ( part1
    , part2
    ) where

import Utils (findAll)

import Data.List (isInfixOf)
import Text.Megaparsec (anyChar, char, noneOf)
import Text.Megaparsec.String (Parser)


splitSupernetsAndHypernets :: String -> ([String], [String])
splitSupernetsAndHypernets = go ([], [])
    where go (sns, hns) input = let (segment, rest) = break (`elem` "[]") input
                                in case rest of
                                     ('[': xs) -> go (segment : sns, hns) xs
                                     (']': xs) -> go (sns, segment : hns) xs
                                     _         -> (segment : sns, hns)

parseAbba :: Parser String
parseAbba = do
  a <- anyChar
  b <- noneOf [a]
  char b
  char a
  return [a, b, b, a]

part1 :: String -> String
part1 = show . length . filter (valid . splitSupernetsAndHypernets) . lines
    where hasAbba = not . null . findAll parseAbba
          valid (sns, hns) = any hasAbba sns && all (not . hasAbba) hns

expectedBab :: Parser String
expectedBab = do
  a <- anyChar
  b <- noneOf [a]
  char a
  return [b, a, b]

part2 :: String -> String
part2 = show . length . filter (valid . splitSupernetsAndHypernets) . lines
    where valid (sns, hns) = let babs = concatMap (findAll expectedBab) sns
                             in or $ isInfixOf <$> babs <*> hns
