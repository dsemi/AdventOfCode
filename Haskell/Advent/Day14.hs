{-# LANGUAGE QuasiQuotes #-}

module Advent.Day14
    ( part1
    , part2
    ) where

import Advent.Problem

import Data.Ord
import Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as M
import qualified Data.IntMultiSet as MS
import Data.List (sortBy, transpose)
import Text.Regex.PCRE.Heavy (re, scan)

totalTime :: Int
totalTime = 2503

getDistancesAtEachSecond :: String -> HashMap String [Int]
getDistancesAtEachSecond input = M.fromList [ (name, take totalTime distStages)
                                            | [name, spd, fT, rT] <- map snd $ scan regex input
                                            , let speed = read spd
                                            , let flyTime = read fT
                                            , let restTime = read rT
                                            , let distStages = scanl1 (+) . cycle
                                                               $ replicate flyTime speed
                                                               ++ replicate restTime 0
                                            ]
    where regex = [re|(\S+) .* (\d+) .* (\d+) .* (\d+) seconds.|]

maxesBy :: Ord b => (a -> b) -> [a] -> [a]
maxesBy cmp xs = let ms = sortBy (flip $ comparing cmp) xs
                 in takeWhile ((== cmp (head ms)) . cmp) ms

part1 :: Problem
part1 = Pure $ maximum . map last . M.elems . getDistancesAtEachSecond

p2 :: String -> Int
p2 input = let dists = getDistancesAtEachSecond input
               counts = MS.fromList . concatMap (map fst . maxesBy snd . zip [1..])
                        . transpose $ M.elems dists
           in snd . last $ MS.toAscOccurList counts

part2 :: Problem
part2 = Pure p2
