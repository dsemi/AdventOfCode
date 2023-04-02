module Year2016.Day07
    ( part1
    , part2
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import FlatParse.Basic

splitSupernetsAndHypernets :: ByteString -> ([ByteString], [ByteString])
splitSupernetsAndHypernets = go ([], [])
    where go (sns, hns) input = let (segment, rest) = B.break (`elem` "[]") input
                                in case B.uncons rest of
                                     Just ('[', xs) -> go (segment : sns, hns) xs
                                     Just (']', xs) -> go (sns, segment : hns) xs
                                     _              -> (segment : sns, hns)

parseAbba :: Parser () ByteString
parseAbba = do
  a <- anyAsciiChar
  b <- satisfy (/= a)
  skipSatisfy (== b) >> skipSatisfy (== a)
  pure $ B.pack [a, b, b, a]

findAll :: Parser () a -> ByteString -> [a]
findAll parser = parse . B.tails
    where parse [] = []
          parse (x:xs) = case runParser parser x of
                           OK res _ -> res : parse xs
                           _ -> parse xs

part1 :: ByteString -> Int
part1 = length . filter (valid . splitSupernetsAndHypernets) . B.lines
    where hasAbba = not . null . findAll parseAbba
          valid (sns, hns) = any hasAbba sns && all (not . hasAbba) hns

expectedBab :: Parser () ByteString
expectedBab = do
  a <- anyAsciiChar
  b <- satisfy (/= a)
  skipSatisfy (== a)
  pure $ B.pack [b, a, b]

part2 :: ByteString -> Int
part2 = length . filter (valid . splitSupernetsAndHypernets) . B.lines
    where valid (sns, hns) = let babs = concatMap (findAll expectedBab) sns
                             in or $ B.isInfixOf <$> babs <*> hns
