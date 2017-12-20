{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Year2017.Day16
    ( part1
    , part2
    ) where

import Data.HashMap.Strict ((!))
import qualified Data.HashMap.Strict as M
import Data.List (foldl')
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Data.Tuple (swap)
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV


spin :: Int -> Vector Char -> Vector Char
spin n v = uncurry (V.++) $ swap $ V.splitAt (V.length v - n) v

exchange :: Int -> Int -> Vector Char -> Vector Char
exchange i j = V.modify $ \v -> do
                 iVal <- MV.read v i
                 jVal <- MV.read v j
                 MV.write v i jVal
                 MV.write v j iVal

partner :: Char -> Char -> Vector Char -> Vector Char
partner a b v = exchange (getIndex a) (getIndex b) v
    where getIndex c = fromJust $ V.elemIndex c v

dance :: Int -> [String] -> Vector Char
dance n m = go M.empty n $ V.fromList ['a'..'p']
    where moves = map parse m
          doDance v = foldl' (flip ($)) v moves
          go _ 0 v = v
          go m' c v
              | M.member k m' = let cycleLen = n - c - m' ! k
                                in iterate doDance v !! (c `mod` cycleLen)
              | otherwise = go (M.insert k (n-c) m') (c-1) $ doDance v
              where k = V.toList v
          parse ('s' : x) = spin (read x)
          parse ('x' : rest) = let [a, b] = splitOn "/" rest
                               in exchange (read a) (read b)
          parse ('p' : rest) = let [a, b] = splitOn "/" rest
                               in partner (head a) (head b)
          parse _ = error "Invalid line"

part1 :: String -> String
part1 = V.toList . dance 1 . splitOn ","

part2 :: String -> String
part2 = V.toList . dance (10^9) . splitOn ","
