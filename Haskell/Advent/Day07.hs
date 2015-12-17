{-# LANGUAGE QuasiQuotes #-}

module Advent.Day07
    ( part1
    , part2
    ) where

import Advent.Problem

import Data.Bits
import Data.HashMap.Lazy (HashMap, (!))
import qualified Data.HashMap.Lazy as M
import Data.List (foldl')
import Data.Maybe
import Data.String.Utils
import Debug.Trace
import Text.Regex.PCRE.Heavy (re, scan)

ops :: HashMap String (Int -> Int -> Int)
ops = M.fromList [ ( "NOT", const complement )
                 , ( "AND", (.&.) )
                 , ( "OR", (.|.) )
                 , ( "LSHIFT", shiftL )
                 , ( "RSHIFT", shiftR )
                 ]
buildWires :: [String] -> HashMap String Int
buildWires input = wires
    where wires = foldl' addWire M.empty input
          regex = [re|(?:(?:(\S+) )?(\S+) )?(\S+) -> (\S+)|]
          addWire :: HashMap String Int -> String -> HashMap String Int
          addWire m s = let [a, op, b, w] = snd . head $ scan regex s
                            op' = fromMaybe (flip const) $ M.lookup op ops
                            a'  = fromMaybe (if null a then 0 else wires ! a) $ maybeRead a
                            b'  = fromMaybe (wires ! b) $ maybeRead b
                        in M.insert w (op' a' b') m

p1 :: String -> Int
p1 = (! "a") . buildWires . lines

part1 :: Problem
part1 = Pure p1

p2 :: String -> Int
p2 input = let wires = buildWires $ lines input ++ [show (p1 input) ++ " -> b"]
           in wires ! "a"

part2 :: Problem
part2 = Pure p2
