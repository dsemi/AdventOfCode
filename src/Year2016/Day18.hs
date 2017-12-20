module Year2016.Day18
    ( part1
    , part2
    ) where

import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V


makeNextRow :: Vector Char -> Vector Char
makeNextRow prevRow = V.imap fun prevRow
    where vLastI = V.length prevRow - 1
          fun i _ = safeOrTrap a b c
              where a = if i == 0 then '.' else V.unsafeIndex prevRow (i-1)
                    b = V.unsafeIndex prevRow i
                    c = if i == vLastI then '.' else V.unsafeIndex prevRow (i+1)
          safeOrTrap '^' '^' '.' = '^'
          safeOrTrap '.' '^' '^' = '^'
          safeOrTrap '^' '.' '.' = '^'
          safeOrTrap '.' '.' '^' = '^'
          safeOrTrap  _   _   _  = '.'

numSafe :: Int -> String -> Int
numSafe n = sum . map (V.length . V.filter (== '.')) . take n . iterate makeNextRow . V.fromList

part1 :: String -> Int
part1 = numSafe 40

part2 :: String -> Int
part2 = numSafe 400000
