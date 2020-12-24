{-# LANGUAGE NegativeLiterals, OverloadedStrings #-}

module Year2020.Day24
    ( part1
    , part2
    ) where

import Data.Bits
import Data.Char
import Data.Maybe
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import Linear.V3
import Text.Megaparsec


dirs :: [V3 Int -> V3 Int]
dirs = [ (+ V3  1 -1  0) -- East
       , (+ V3  0 -1  1) -- Southeast
       , (+ V3 -1  0  1) -- Southwest
       , (+ V3 -1  1  0) -- West
       , (+ V3  0  1 -1) -- Northwest
       , (+ V3  1  0 -1) -- Northeast
       ]

flipTiles :: String -> HashMap (V3 Int) Bool
flipTiles = M.filter id . M.fromListWith xor . map ((,True) . foldr ($) (V3 0 0 0) . reverse)
            . map (fromJust . parseMaybe @() pDirs) . lines . map toUpper
    where pDirs = some $ choice $ zipWith (<$) dirs ["E", "SE", "SW", "W", "NW", "NE"]

part1 :: String -> Int
part1 = M.size . flipTiles

step :: HashMap (V3 Int) Bool -> HashMap (V3 Int) Bool
step m = M.filter id $ M.mapWithKey black adj
    where adj = M.fromListWith (+) $ map (,1) $ concatMap neighbors $ M.keys m
          black k v = if M.lookupDefault False k m then v /= 0 && v <= 2 else v == 2
          neighbors x = map ($ x) dirs

part2 :: String -> Int
part2 = M.size . (!! 100) . iterate step . flipTiles
