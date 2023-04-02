module Year2016.Day20
    ( part1
    , part2
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.List (sort)
import FlatParse.Basic

collapseIpFilters :: ByteString -> [(Int, Int)]
collapseIpFilters = go . sort . map parse . B.lines
    where parse line =
              case runParser ((,) <$> anyAsciiDecimalInt <* $(char '-') <*> anyAsciiDecimalInt) line of
                OK res _ -> res
                _ -> error "unreachable"
          go [] = []
          go [y] = [y]
          go ((low0, high0) : (low1, high1) : rest)
              | low1 <= high0 + 1 = go $ (low0, max high0 high1) : rest
              | otherwise = (low0, high0) : go ((low1, high1) : rest)

part1 :: ByteString -> Int
part1 = (\(a, b) -> if a > 0 then 0 else b+1) . head . collapseIpFilters

part2 :: ByteString -> Int
part2 = (2^32 -) . sum . map ((+1) . abs . uncurry (-)) . collapseIpFilters
