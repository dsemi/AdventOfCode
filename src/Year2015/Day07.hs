{-# LANGUAGE OverloadedStrings #-}

module Year2015.Day07
    ( part1
    , part2
    ) where

import Data.Bits (complement, shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.HashMap.Lazy ((!), HashMap, fromList, insert)
import Data.Word (Word16)
import FlatParse.Basic

parseNode :: (Either ByteString Word16 -> Word16) -> ByteString -> (ByteString, Word16)
parseNode f line =
    case B.words line of
      [                          (operand -> b), "->", v] -> (v, f b)
      [                "NOT",    (operand -> b), "->", v] -> (v, complement $ f b)
      [(operand -> a), "AND",    (operand -> b), "->", v] -> (v, f a .&. f b)
      [(operand -> a), "OR",     (operand -> b), "->", v] -> (v, f a .|. f b)
      [(operand -> a), "LSHIFT", (operand -> b), "->", v] -> (v, f a .<<. f b)
      [(operand -> a), "RSHIFT", (operand -> b), "->", v] -> (v, f a .>>. f b)
      _ -> error "Invalid line"
    where operand :: ByteString -> Either ByteString Word16
          operand x = case runParser anyAsciiDecimalInt x of
                        OK n _ -> Right $ fromIntegral n
                        _ -> Left x
          (.<<.) a b = shiftL a $ fromIntegral b
          (.>>.) a b = shiftR a $ fromIntegral b

build :: (ByteString -> Word16) -> ByteString -> HashMap ByteString Word16
build f = fromList . map (parseNode (either f id)) . B.lines

part1 :: ByteString -> Word16
part1 input = let m = build (m !) input
              in m ! "a"

part2 :: ByteString -> Word16
part2 input = let m = insert "b" (part1 input) $ build (m !) input
              in m ! "a"
