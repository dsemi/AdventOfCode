{-# LANGUAGE BangPatterns #-}

module Year2021.Day25
    ( part1
    , part2
    ) where

import Data.Bits
import Data.Bool
import Data.List (foldl')

part1 :: String -> Int
part1 input = go 1 (map (toInt '>') grid) (map (toInt 'v') grid)
    where grid = lines input
          mc = length (head grid)
          toInt :: Char -> String -> Integer
          toInt c = foldl' (\a b -> shiftL a 1 .|. bool 0 1 (b == c)) 0
          go !cnt horz vert =
              let horz' = map rot horz
                  hColls = zipWith (.&.) horz' $ zipWith (.|.) horz vert
                  prevHColls = map (\n -> clearBit (shiftL n 1) mc
                                    .|. (if testBit n (mc-1) then bit 0 else 0)) hColls
                  horz'' = zipWith3 (\a b c -> (a `xor` b) .|. c) horz' hColls prevHColls
                  vert' = last vert : init vert
                  vColls = zipWith (.&.) vert' $ zipWith (.|.) horz'' vert
                  prevVColls = tail vColls ++ [head vColls]
                  vert'' = zipWith3 (\a b c -> (a `xor` b) .|. c) vert' vColls prevVColls
              in if horz == horz'' && vert == vert'' then cnt
                 else go (cnt+1) horz'' vert''
          rot n = shiftR n 1 .|. (if testBit n 0 then bit (mc-1) else 0)

part2 :: String -> String
part2 = const ""
