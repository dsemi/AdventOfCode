module Year2015.Day14
    ( part1
    , part2
    ) where

import Utils

import Data.Bool
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.List (transpose)


getDistancesAtEachSecond :: ByteString -> [[Int]]
getDistancesAtEachSecond input =
    [ take 2503 . scanl1 (+) . cycle $ replicate flyTime speed ++ replicate restTime 0
    | [speed, flyTime, restTime] <- map findAllInts $ B.lines input
    ]

part1 :: ByteString -> Int
part1 = maximum . map last . getDistancesAtEachSecond

part2 :: ByteString -> Int
part2 = maximum . map sum . transpose . map (\xs -> let m = maximum xs in map (bool 0 1 . (==m)) xs)
        . transpose . getDistancesAtEachSecond
