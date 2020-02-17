module Year2015.Day07
    ( part1
    , part2
    ) where

import Data.Bits (complement, shiftL, shiftR, (.&.), (.|.))
import Data.Maybe (fromJust)
import Data.Word (Word16)

import Utils


parseNode :: (Either String Word16 -> Word16) -> String -> (String, Word16)
parseNode f line =
    case words line of
      [                        (parse -> a), "->", v] -> (v, f a)
      [              "NOT",    (parse -> a), "->", v] -> (v, complement $ f a)
      [(parse -> a), "AND",    (parse -> b), "->", v] -> (v, f a .&. f b)
      [(parse -> a), "OR",     (parse -> b), "->", v] -> (v, f a .|. f b)
      [(parse -> a), "LSHIFT", (parse -> b), "->", v] -> (v, f a .<<. f b)
      [(parse -> a), "RSHIFT", (parse -> b), "->", v] -> (v, f a .>>. f b)
      _ -> error "Invalid line"
    where parse :: String -> Either String Word16
          parse x = maybe (Left x) Right $ maybeRead x
          (.<<.) a b = shiftL a $ fromIntegral b
          (.>>.) a b = shiftR a $ fromIntegral b

(!) :: (Eq a) => [(a, b)] -> a -> b
m ! k = fromJust $ lookup k m

build :: (String -> Word16) -> String -> [(String, Word16)]
build f = map (parseNode (either f id)) . lines

part1 :: String -> Word16
part1 input = let m = build (m !) input
              in m ! "a"

part2 :: String -> Word16
part2 input = let m = ("b", part1 input) : build (m !) input
              in m ! "a"
