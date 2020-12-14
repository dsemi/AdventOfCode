module Year2020.Day14
    ( part1
    , part2
    ) where

import Data.Bits
import qualified Data.Map as M


parse :: String -> [([(Int, Char)], Int, Int)]
parse = go [] . lines
    where go _ [] = []
          go mask ((words -> line):xs)
              | head line == "mask" = go (zip [35,34..] (last line)) xs
              | otherwise = let r = read $ init $ drop 4 $ head line
                                v = read $ last line
                            in (mask, r, v) : go mask xs

part1 :: String -> Int
part1 = sum . M.elems . M.fromList . map process . parse
    where process (mask, r, v) = (r, foldr go v mask)
          go (i, '1') = (`setBit` i)
          go (i, '0') = (`clearBit` i)
          go (_, _)   = id

part2 :: String -> Int
part2 = sum . M.elems . M.fromList . concatMap process . parse
    where process (mask, r, v) = map (,v) $ foldr go [r] mask
          go (i, '1') ns = (`setBit` i) <$> ns
          go (_, '0') ns = ns
          go (i, _)   ns = [(`clearBit` i), (`setBit` i)] <*> ns
