module Year2016.Day15
    ( part1
    , part2
    ) where

import Utils


parseDisc :: String -> (Integer, Integer)
parseDisc line =
    case findAllInts line of
      [discNum, modulo, _, pos] -> (-pos - discNum, modulo)
      _ -> error "Parse error"

part1 :: String -> Maybe Integer
part1 input = chineseRemainder discs
    where discs = map parseDisc $ lines input

part2 :: String -> Maybe Integer
part2 input = chineseRemainder discs
    where discs = map parseDisc $ lines $ input ++ extra
          extra = "\nDisc #7 has 11 positions; at time=0, it is at position 0."
