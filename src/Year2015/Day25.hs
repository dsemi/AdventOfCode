{-# LANGUAGE DataKinds #-}

module Year2015.Day25
    ( part1
    , part2
    ) where

import Utils

import Math.NumberTheory.Moduli.Class (Mod, getVal, (^%))


parseCoord :: String -> (Int, Int)
parseCoord input = let (Right [r, c]) = findAllInts input
                   in (r, c)

makeCode :: Int -> Int
makeCode n = fromInteger . getVal $ (252533 :: Mod 33554393) ^% n * 20151125

part1 :: String -> Int
part1 input = makeCode index
    where (r, c) = parseCoord input
          n = r + c - 1
          index = n * (n - 1) `div` 2 + c - 1

part2 :: String -> String
part2 = const ""
