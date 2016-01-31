{-# LANGUAGE QuasiQuotes #-}

module Advent.Day07
    ( part1
    , part2
    ) where

import Advent.Problem

import Data.Bits
import Data.Either
import Data.Function.Memoize
import Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as M
import Data.List (foldl')
import Data.Maybe
import Data.Either.Utils
import Data.String.Utils
import Text.Regex.PCRE.Heavy (re, scan)

type Id = Either String Int
data Node = Const Id
          | Not Id
          | And Id Id
          | Or Id Id
          | LShift Id Id
          | RShift Id Id

parseNode :: String -> Id -> Id -> Node
parseNode ""       _ b = Const b
parseNode "NOT"    _ b = Not b
parseNode "AND"    a b = And a b
parseNode "OR"     a b = Or a b
parseNode "LSHIFT" a b = LShift a b
parseNode "RSHIFT" a b = RShift a b

eval :: (String -> Int) -> Node -> Int
eval f exp = case exp of
               Const a    -> val a
               Not a      -> complement $ val a
               And a b    -> val a .&. val b
               Or a b     -> val a .|. val b
               LShift a b -> val a `shiftL` val b
               RShift a b -> val a `shiftR` val b
    where val = either f id


buildWires :: [String] -> HashMap String Node
buildWires input = foldl' addWire M.empty input
    where regex = [re|(?:(?:(\S+) )?(\S+) )?(\S+) -> (\S+)|]
          addWire :: HashMap String Node -> String -> HashMap String Node
          addWire m s = let [a, op, b, w] = snd . head $ scan regex s
                            a' = maybeToEither a $ maybeRead a
                            b' = maybeToEither b $ maybeRead b
                            node = parseNode op a' b'
                        in M.insert w node m

getValue :: HashMap String Node -> String -> Int
getValue m k = mgv k
    where mgv = memoFix gv
          gv f k = eval f $ m ! k

p1 :: String -> Int
p1 input = let m = buildWires $ lines input
           in getValue m "a"

part1 :: Problem
part1 = Pure p1

p2 :: String -> Int
p2 input = let m = buildWires $ lines input
               a = getValue m "a"
               m' = M.insert "b" (Const $ Right a) m
           in getValue m' "a"

part2 :: Problem
part2 = Pure p2
