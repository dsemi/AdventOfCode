module Year2015.Day09
    ( part1
    , part2
    ) where

import Data.Array.Unboxed
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.HashMap.Strict as M
import Data.List (nub)
import Data.Tuple
import FlatParse.Basic

import Utils

parseLine :: ByteString -> ((ByteString, ByteString), Int)
parseLine line = case runParser parser line of
                   OK edge _ -> edge
                   _ -> error "unreachable"
    where letter = byteStringOf (some $ satisfy (\x -> isDigit x || isLatinLetter x))
          parser = (,) <$> ((,) <$> letter <* $(string " to ")
                           <*> letter <* $(string " = "))
                   <*> anyAsciiDecimalInt

adjMap :: ByteString -> UArray (Int, Int) Int
adjMap input = let m = M.fromList $ concatMap (\line -> let (k, v) = parseLine line
                                                        in [(k, v), (swap k, v)]) $ B.lines input
                   idx = M.fromList $ zip (nub $ map fst $ M.keys m) [1..]
                   len = M.size idx
               in array ((0, 0), (len, len)) [((idx M.! a, idx M.! b), v) | ((a, b), v) <- M.toList m]

part1 :: ByteString -> Int
part1 = heldKarp min . adjMap

part2 :: ByteString -> Int
part2 = heldKarp max . adjMap
