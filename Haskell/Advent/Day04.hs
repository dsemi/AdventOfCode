module Advent.Day04
    ( part1
    , part2
    ) where

import Advent.Problem

import Data.Hash.MD5
import Data.List (findIndex)
import Data.Maybe
import Data.String.Utils

concatNumsToMD5 :: String -> [String]
concatNumsToMD5 s = map (md5s . Str . (s ++) . show) [0..]

part1 :: Problem
part1 = Pure $ fromJust . findIndex (startswith "00000") . concatNumsToMD5

part2 :: Problem
part2 = Pure $ fromJust . findIndex (startswith "000000") . concatNumsToMD5
