{-# LANGUAGE OverloadedLists #-}

module Year2017.Day10
    ( part1
    , part2
    ) where

import Control.Lens
import Data.Bits (xor)
import Data.ByteString (ByteString, pack)
import Data.ByteString.Base16 (encode)
import Data.Char (ord)
import Data.List (foldl')
import Data.List.Split (chunksOf, splitOn)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word8)


hash :: Int -> [Int] -> Vector Word8
hash n lengths = view _3 $ (!! n) $ iterate (flip (foldl' f) lengths) (0, 0, [0..255])
    where f (i, ss, v) x = (i + x + ss, ss + 1, V.update_ v idcs vals)
              where idcs = V.map (`mod` V.length v) [i .. i+x-1]
                    vals = V.backpermute v $ V.reverse idcs

part1 :: String -> Int
part1 = V.product . V.map fromIntegral . V.take 2 . hash 1 . map read . splitOn ","

part2 :: String -> ByteString
part2 = encode . pack . map (foldr1 xor) . chunksOf 16 . V.toList . hash 64
        . (++[17, 31, 73, 47, 23]) . map ord
