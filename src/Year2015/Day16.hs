{-# LANGUAGE OverloadedStrings #-}

module Year2015.Day16
    ( part1
    , part2
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.List (findIndex)
import Data.Maybe
import FlatParse.Basic

tape :: [(ByteString, (Int -> Bool))]
tape = [ ("children", (==3))
       , ("cats", (==7))
       , ("samoyeds", (==2))
       , ("pomeranians", (==3))
       , ("akitas", (==0))
       , ("vizslas", (==0))
       , ("goldfish", (==5))
       , ("trees", (==3))
       , ("cars", (==2))
       , ("perfumes", (==1))
       ]

parseLines :: ByteString -> [[(ByteString, Int)]]
parseLines = map parse . B.lines
    where cnt = (,) <$> byteStringOf (some $ satisfy isLatinLetter) <* $(string ": ")
                <*> anyAsciiDecimalInt <* optional_ $(string ", ")
          parser = $(string "Sue ") >> skipSome (satisfy isDigit) >> $(string ": ") >> some cnt
          parse line = case runParser parser line of
                         OK c _ -> c
                         _ -> error "unreachable"

-- Sue 480: goldfish: 1, children: 9, vizslas: 3
solve :: [(ByteString, (Int -> Bool))] -> ByteString -> Maybe Int
solve tape' = fmap succ . findIndex couldMatch . parseLines
    where couldMatch = all (uncurry (fromJust . (`lookup` tape')))

part1 :: ByteString -> Maybe Int
part1 = solve tape

part2 :: ByteString -> Maybe Int
part2 = solve $ [ ("cats", (>7))
                , ("pomeranians", (<3))
                , ("goldfish", (<5))
                , ("trees", (>3))
                ] ++ tape
