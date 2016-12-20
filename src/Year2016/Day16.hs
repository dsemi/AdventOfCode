{-# LANGUAGE OverloadedStrings #-}

module Year2016.Day16
    ( part1
    , part2
    ) where

import qualified Data.ByteString.Char8 as B


dragonChecksum :: Int -> String -> String
dragonChecksum desiredLen ns = until (odd . length) checksum . B.unpack . B.take desiredLen
                               . until ((>=desiredLen) . B.length) curve $ B.pack ns
    where curve a = mconcat [a, "0", B.map rep $ B.reverse a]
          rep '1' = '0'
          rep '0' = '1'
          checksum [] = []
          checksum (a:b:rest)
              | a == b    = '1' : checksum rest
              | otherwise = '0' : checksum rest

part1 :: String -> String
part1 = dragonChecksum 272

part2 :: String -> String
part2 = dragonChecksum 35651584
