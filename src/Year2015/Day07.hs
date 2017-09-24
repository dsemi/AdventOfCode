module Year2015.Day07
    ( part1
    , part2
    ) where

import Data.Bits
import Data.Either.Utils
import Data.Maybe
import Data.String.Utils
import Data.Word


parseNode :: (String -> Word16) -> String -> (String, Word16)
parseNode f line =
    case words line of
      [             a, "->", v] -> (v, fn f a)
      [   "NOT",    a, "->", v] -> (v, complement $ fn f a)
      [a, "AND",    b, "->", v] -> (v, fn f a .&. fn f b)
      [a, "OR",     b, "->", v] -> (v, fn f a .|. fn f b)
      [a, "LSHIFT", b, "->", v] -> (v, fn f a `shiftL` fromIntegral (fn f b))
      [a, "RSHIFT", b, "->", v] -> (v, fn f a `shiftR` fromIntegral (fn f b))
    where fn f x = either f id . maybeToEither x $ maybeRead x

m ! k = fromJust $ lookup k m
build m = map (parseNode (m !)) . lines

part1 :: String -> Word16
part1 input = let m = build m input
              in m ! "a"

part2 :: String -> Word16
part2 input = let m = build m input
                  m' = ("b", (m ! "a")) : build m' input
              in m' ! "a"
