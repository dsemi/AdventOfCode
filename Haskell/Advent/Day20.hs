module Advent.Day20
    ( part1
    , part2
    ) where

import Advent.Problem

import qualified Data.Set as S
import Math.NumberTheory.Primes.Factorisation

p1 :: String -> Int
p1 input = fromInteger . head $ dropWhile ((<minPresents) . (*10) . divisorSum) [1..]
    where minPresents = read input

part1 :: Problem
part1 = Pure p1

p2 :: String -> Int
p2 input =  fromInteger . head $ dropWhile (\n -> (<minPresents) . (*11) . S.foldl' (+) 0
                                                  . S.filter ((>=n) . (*50)) $ divisors n) [1..]
    where minPresents = read input

part2 :: Problem
part2 = Pure p2
