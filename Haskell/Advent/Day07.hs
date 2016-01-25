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

data Node = Const Int
          | Op (Int -> Int -> Int) (Either String Int) (Either String Int)

ops :: HashMap String (Int -> Int -> Int)
ops = M.fromList [ ( "NOT", const complement )
                 , ( "AND", (.&.) )
                 , ( "OR", (.|.) )
                 , ( "LSHIFT", shiftL )
                 , ( "RSHIFT", shiftR )
                 ]

buildWires :: [String] -> HashMap String Node
buildWires input = foldl' addWire M.empty input
    where regex = [re|(?:(?:(\S+) )?(\S+) )?(\S+) -> (\S+)|]
          addWire :: HashMap String Node -> String -> HashMap String Node
          addWire m s = let [a, op, b, w] = snd . head $ scan regex s
                            op' = fromMaybe (flip const) $ M.lookup op ops
                            a'  = parseA a
                            b'  = maybeToEither b (maybeRead b)
                        in M.insert w (Op op' a' b') m
          parseA :: String -> Either String Int
          parseA "" = Right 0
          parseA a  = maybeToEither a (maybeRead a)

getValue :: HashMap String Node -> String -> Int
getValue m k = mgv k
    where mgv = memoFix gv
          gv f k = case (m ! k) of
                     Op op a b -> let a' = either f id a
                                      b' = either f id b
                                  in op a' b'
                     Const n   -> n

p1 :: String -> Int
p1 input = let m = buildWires $ lines input
           in getValue m "a"

part1 :: Problem
part1 = Pure p1

p2 :: String -> Int
p2 input = let m = buildWires $ lines input
               a = getValue m "a"
               m' = M.insert "b" (Const a) m
           in getValue m' "a"

part2 :: Problem
part2 = Pure p2
