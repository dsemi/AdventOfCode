{-# LANGUAGE OverloadedStrings #-}

module Advent.Day12
    ( part1
    , part2
    ) where

import Data.Aeson
import Data.ByteString.Lazy.Char8 (ByteString, pack)
import Data.HashMap.Strict (elems)
import Data.Maybe
import Data.Text (Text)
import qualified Data.Vector as V

readJson :: ByteString -> Value
readJson = fromMaybe (Number 0) . decode

sumNumbers :: Maybe Text -> Value -> Int
sumNumbers Nothing (Object m) = sum . map (sumNumbers Nothing) $ elems m
sumNumbers noval@(Just s) (Object m)
    | String s `elem` m       = 0
    | otherwise               = sum . map (sumNumbers noval) $ elems m
sumNumbers noval (Array a)    = V.sum $ V.map (sumNumbers noval) a
sumNumbers noval (Number n)   = truncate n
sumNumbers noval (String s)   = 0
sumNumbers noval (Bool n)     = 0

part1 :: String -> String
part1 = show . sumNumbers Nothing . readJson . pack

part2 :: String -> String
part2 = show . sumNumbers (Just "red") . readJson . pack
