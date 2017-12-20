{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedStrings #-}

module Year2016.Day05
    ( part1
    , part2
    ) where

import Utils

import Crypto.Hash.MD5 (hash)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Base16 (encode)
import Data.Conduit
import qualified Data.Conduit.List as L
import Data.Maybe
import Data.Monoid ((<>))
import qualified Data.HashMap.Strict as M


part1 :: ByteString -> IO String
part1 seed = map (B.head . B.drop 5 . hashNum) <$> (multi f (+1) 0 $$ L.take 8)
    where hashNum = encode . hash . (seed <>) . B.pack . show
          f = ("00000" `B.isPrefixOf`) . hashNum

part2 :: ByteString -> IO ByteString
part2 seed = multi f (+1) 0 $$ findPassword M.empty
    where hashNum = encode . hash . (seed <>) . B.pack . show
          f = ("00000" `B.isPrefixOf`) . hashNum
          indices = "01234567"
          addChar m pos char
              | pos `elem` indices && not (M.member pos m) = M.insert pos char m
              | otherwise                                  = m
          findPassword m
              | M.size m == 8 = return . B.pack $ map (m M.!) indices
              | otherwise = do
                  [p, c] <- B.unpack . B.take 2 . B.drop 5 . hashNum . fromJust <$> await
                  findPassword $ addChar m p c
