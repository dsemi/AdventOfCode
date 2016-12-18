module Year2016.Day18
    ( part1
    , part2
    ) where

makeNextRow :: String -> String
makeNextRow prevRow@(_:rest) = zipWith3 safeOrTrap ('.':prevRow) prevRow $ rest ++ "."
    where safeOrTrap '^' '^' '.' = '^'
          safeOrTrap '.' '^' '^' = '^'
          safeOrTrap '^' '.' '.' = '^'
          safeOrTrap '.' '.' '^' = '^'
          safeOrTrap  _   _   _  = '.'

numSafe :: Int -> String -> Int
numSafe n = length . filter (=='.') . concat . take n . iterate makeNextRow

part1 :: String -> Int
part1 = numSafe 40

part2 :: String -> Int
part2 = numSafe 400000
