{-# LANGUAGE OverloadedStrings #-}

module Year2016.Day05
    ( part1
    , part2
    ) where

import Utils

import Control.Parallel.Strategies
import Crypto.Hash.MD5 (hash)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Base16 (encode)
import Data.Monoid ((<>))
import qualified Data.HashMap.Strict as M


part1 :: ByteString -> IO String
part1 seed = parallel $ map (B.head . B.drop 5) $ take 8 $ filter f
             $ runEval $ parBuffer 1000 rseq $ map hashNum [0..]
    where hashNum :: Int -> ByteString
          hashNum = encode . hash . (seed <>) . B.pack . show
          f = ("00000" `B.isPrefixOf`)

part2 :: ByteString -> IO ByteString
part2 seed = parallel $ findPassword M.empty $ filter f
             $ runEval $ parBuffer 1000 rseq $ map hashNum [0..]
    where hashNum :: Int -> ByteString
          hashNum = encode . hash . (seed <>) . B.pack . show
          f = ("00000" `B.isPrefixOf`)
          indices = "01234567"
          addChar m pos char
              | pos `elem` indices && not (M.member pos m) = M.insert pos char m
              | otherwise                                  = m
          findPassword m [] = B.pack $ map (m M.!) indices
          findPassword m (x:xs)
              | M.size m == 8 = findPassword m []
              | otherwise =
                  let [p, c] = B.unpack $ B.take 2 $ B.drop 5 x
                  in findPassword (addChar m p c) xs
