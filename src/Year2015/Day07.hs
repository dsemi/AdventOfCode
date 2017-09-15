module Year2015.Day07
    ( part1
    , part2
    ) where

import Data.Bits
import Data.HashMap.Lazy (HashMap, fromList, insert, (!))
import Data.Either.Utils
import Data.String.Utils
import Data.Word


parseNode :: (String -> Word16) -> String -> (String, Word16)
parseNode f line =
    case words line of
      [             a, "->", v] -> (v, fn a f)
      [   "NOT",    a, "->", v] -> (v, complement $ fn a f)
      [a, "AND",    b, "->", v] -> (v, fn a f .&. fn b f)
      [a, "OR",     b, "->", v] -> (v, fn a f .|. fn b f)
      [a, "LSHIFT", b, "->", v] -> (v, fn a f `shiftL` fromIntegral (fn b f))
      [a, "RSHIFT", b, "->", v] -> (v, fn a f `shiftR` fromIntegral (fn b f))
    where fn x = either (flip ($)) const . maybeToEither x $ maybeRead x

buildGraph :: HashMap String Word16 -> String -> HashMap String Word16
buildGraph m = fromList . map (parseNode (m !)) . lines

p1 :: String -> Word16
p1 input = let m = buildGraph m input
           in m ! "a"

part1 :: String -> String
part1 = show . p1

p2 :: String -> Word16
p2 input = let m = buildGraph m input
               m' = insert "b" (m ! "a") $ buildGraph m' input
           in m' ! "a"

part2 :: String -> String
part2 = show . p2
