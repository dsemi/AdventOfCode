{-# LANGUAGE ViewPatterns #-}

module Year2015.Day07
    ( part1
    , part2
    ) where

import Data.Bits (complement, shiftL, shiftR, (.&.), (.|.))
import Data.Either.Utils (maybeToEither)
import Data.Maybe (fromJust)
import Data.String.Utils (maybeRead)
import Data.Word (Word16)


parseNode :: (Either String Word16 -> Word16) -> String -> (String, Word16)
parseNode f line =
    case words line of
      [                        (parse -> a), "->", v] -> (v, f a)
      [              "NOT",    (parse -> a), "->", v] -> (v, complement $ f a)
      [(parse -> a), "AND",    (parse -> b), "->", v] -> (v, f a .&. f b)
      [(parse -> a), "OR",     (parse -> b), "->", v] -> (v, f a .|. f b)
      [(parse -> a), "LSHIFT", (parse -> b), "->", v] -> (v, f a `shiftL` fromIntegral (f b))
      [(parse -> a), "RSHIFT", (parse -> b), "->", v] -> (v, f a `shiftR` fromIntegral (f b))
    where parse :: String -> Either String Word16
          parse x = maybeToEither x $ maybeRead x

m ! k = fromJust $ lookup k m
build m = map (parseNode (either (m !) id)) . lines

part1 :: String -> Word16
part1 input = let m = build m input
              in m ! "a"

part2 :: String -> Word16
part2 input = let m = ("b", part1 input) : build m input
              in m ! "a"
