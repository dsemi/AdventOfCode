{-# LANGUAGE OverloadedStrings #-}

module Advent.Day12
    ( part1
    , part2
    ) where

import Data.Aeson
import Data.ByteString.Lazy.Char8 (ByteString, pack)
import Data.HashMap.Strict (elems)
import Data.Maybe
import Data.Scientific
import qualified Data.Vector as V

readJson :: ByteString -> Maybe Value
readJson = decode

getNumbers :: Maybe Value -> Value -> Scientific
getNumbers noval (Object m) = let vals = elems m
                              in if isJust noval && fromJust noval `elem` vals then 0
                                 else sum . map (getNumbers noval) $ elems m
getNumbers noval (Array a)  = V.sum $ V.map (getNumbers noval) a
getNumbers noval (Number n) = n
getNumbers noval (String s) = 0
getNumbers noval (Bool n)   = 0

part1 :: String -> String
part1 = show . getNumbers Nothing . fromMaybe (Number 0) . readJson . pack

part2 :: String -> String
part2 = show . getNumbers (Just $ String "red") . fromMaybe (Number 0) . readJson . pack
