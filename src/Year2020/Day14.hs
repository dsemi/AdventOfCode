module Year2020.Day14
    ( part1
    , part2
    ) where

import Utils

import Control.Lens
import Data.Bits
import Data.List (isPrefixOf)
import qualified Data.Map as M


parse :: String -> [([(Int, Char)], Int, Int)]
parse = concat . foldr go [[]] . lines
    where go _ [] = error "bad parse"
          go line (x:xs)
              | "mask" `isPrefixOf` line = [] : map (_1 .~ zip [35,34..] (last (words line))) x : xs
              | otherwise = let [r, v] = findAllInts line
                            in ((undefined, r, v) : x) : xs

part1 :: String -> Int
part1 = sum . M.elems . M.fromList . map process . parse
    where process (mask, r, v) = (r, foldr go v mask)
          go (i, m) v = case m of
                          '1' -> setBit v i
                          '0' -> clearBit v i
                          _ -> v

part2 :: String -> Int
part2 = sum . M.elems . M.fromList . concatMap process . parse
    where process (mask, r, v) = map (,v) $ go mask [0] r
          go [] ns _ = ns
          go ((i, m):ms) ns r = go ms ((\b v -> v*2 + b) <$> bits <*> ns) r
              where bits = case m of
                             '1' -> [1]
                             '0' -> [fromEnum (testBit r i)]
                             _ -> [0, 1]
