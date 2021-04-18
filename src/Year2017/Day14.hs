{-# LANGUAGE OverloadedLists #-}

module Year2017.Day14
    ( part1
    , part2
    ) where

import Control.Lens
import Data.Array
import Data.Bits (popCount, testBit, xor)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Char (ord)
import Data.HashSet (HashSet)
import qualified Data.HashSet as S
import Data.List (foldl')
import Data.List.Split (chunksOf)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word8)


knotHash :: String -> ByteString
knotHash = B.pack . map (foldr1 xor) . chunksOf 16 . V.toList . hash 64
           . (++[17, 31, 73, 47, 23]) . map ord
    where hash :: Int -> [Int] -> Vector Word8
          hash n lengths = view _3 $ (!! n) $ iterate (flip (foldl' f) lengths) (0, 0, [0..255])
              where f (i, ss, v) x = (i + x + ss, ss + 1, V.update_ v idcs vals)
                        where idcs = V.map (`mod` V.length v) [i .. i+x-1]
                              vals = V.backpermute v $ V.reverse idcs

hashes :: String -> [ByteString]
hashes key = map (knotHash . ((key ++ "-") ++) . show) ([0..127] :: [Int])

part1 :: String -> Int
part1 = sum . map (B.foldl' (\c x -> c + popCount x) 0) . hashes

grid :: [ByteString] -> Array (Int, Int) Bool
grid bss = listArray ((0, 0), (127, 127))
           [ b | bs <- map B.unpack bss
           , w <- bs
           , b <- map (testBit w) [7,6..0] ]

adjacents :: (Int, Int) -> [(Int, Int)]
adjacents (x, y) = [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]

regionContaining :: Array (Int, Int) Bool -> (Int, Int) -> HashSet (Int, Int)
regionContaining arr i = go S.empty [i]
    where go s [] = s
          go s (x:xs)
              | not (arr ! x) || S.member x s = go s xs
              | otherwise = go (S.insert x s) $ xs ++ filter (inRange (bounds arr)) (adjacents x)

part2 :: String -> Int
part2 key = let arr = grid $ hashes $ key
            in fst $ foldl' (\(c, s) x -> if not (arr ! x) || S.member x s
                                          then (c, s)
                                          else (c+1, S.union s $ regionContaining arr x))
                   (0, S.empty) $ range (bounds arr)
