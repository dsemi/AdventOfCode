{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedStrings #-}

module Year2016.Day05
    ( part1
    , part2
    ) where

import Crypto.Hash.MD5 (hash)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Base16 (encode)
import Data.Monoid ((<>))
import qualified Data.HashMap.Strict as M


-- TODO: parallel
part1 :: ByteString -> String
part1 seed = map (B.head . B.drop 5 . hashNum) $ take 8 $ filter f [0..]
    where hashNum = encode . hash . (seed <>) . B.pack . show
          f = ("00000" `B.isPrefixOf`) . hashNum

-- TODO: parallel
part2 :: ByteString -> ByteString
part2 seed = findPassword M.empty $ filter f [0..]
    where hashNum = encode . hash . (seed <>) . B.pack . show
          f = ("00000" `B.isPrefixOf`) . hashNum
          indices = "01234567"
          addChar m pos char
              | pos `elem` indices && not (M.member pos m) = M.insert pos char m
              | otherwise                                  = m
          findPassword m [] = B.pack $ map (m M.!) indices
          findPassword m (x:xs)
              | M.size m == 8 = findPassword m []
              | otherwise =
                  let [p, c] = B.unpack . B.take 2 . B.drop 5 $ hashNum x
                  in findPassword (addChar m p c) xs
