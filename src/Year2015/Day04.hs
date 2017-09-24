{-# LANGUAGE OverloadedStrings #-}

module Year2015.Day04
    ( part1
    , part2
    ) where

import Crypto.Hash.MD5
import Data.ByteString (ByteString, isPrefixOf, pack)
import qualified Data.ByteString.Char8 as B
import Data.Monoid ((<>))


findHash :: (ByteString -> Bool) -> ByteString -> Int
findHash f seed = head $ filter (f . hash . (seed <>) . B.pack . show) [0..]

part1 :: String -> Int
part1 = findHash f . B.pack
    where zero5s = [pack [0,0,x] | x <- [0..15]]
          f x = any (`isPrefixOf` x) zero5s

part2 :: String -> Int
part2 = findHash f . B.pack
    where zero6 = pack [0,0,0]
          f = (zero6 `isPrefixOf`)
