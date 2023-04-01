{-# LANGUAGE OverloadedStrings #-}

module Year2015.Day05
    ( part1
    , part2
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B

part1 :: ByteString -> Int
part1 = length . filter isNice . B.lines
    where isNice s = not (any (`B.isInfixOf` s) ["ab", "cd", "pq", "xy"])
                     && B.length (B.filter (`B.elem` "aeiou") s) > 2
                     && or (B.zipWith (==) s (B.tail s))

part2 :: ByteString -> Int
part2 = length . filter isNice2 . B.lines
    where f s = not $ null [ () | i <- [0 .. len - 4]
                           , j <- [i + 2 .. len - 2]
                           , idx i == idx j && idx (i+1) == idx (j+1) ]
              where len = B.length s
                    idx = B.index s
          g s = or $ B.zipWith (==) s $ B.drop 2 s
          isNice2 s = f s && g s
