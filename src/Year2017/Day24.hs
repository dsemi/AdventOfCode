{-# LANGUAGE RecordWildCards #-}

module Year2017.Day24
    ( part1
    , part2
    ) where

import Control.Arrow
import Data.List (delete, maximumBy)
import Data.List.Split
import Data.Ord


data Bridge = Bridge { strength :: !Int, len :: !Int, pins :: Int }
data Pipe = Pipe { a :: Int, b :: Int } deriving (Eq)

connect :: Bridge -> Pipe -> Maybe Bridge
connect (Bridge {..}) (Pipe {..})
    | pins == a = Just $ Bridge (strength + a + b) (len + 1) b
    | pins == b = Just $ Bridge (strength + a + b) (len + 1) a
    | otherwise = Nothing

parsePipes :: String -> [Pipe]
parsePipes = map parse . lines
    where parse inp = let [x, y] = splitOn "/" inp
                      in Pipe (read x) (read y)

combos :: Bridge -> [Pipe] -> [Bridge]
combos bridge [] = [bridge]
combos bridge ps = [ bridge'' | pipe <- ps
                   , Just bridge' <- pure $ connect bridge pipe
                   , bridge'' <- bridge' : combos bridge' (delete pipe ps)
                   ]

part1 :: String -> Int
part1 = maximum . map strength . combos (Bridge 0 0 0) . parsePipes

part2 :: String -> Int
part2 = strength . maximumBy (comparing (len &&& strength)) . combos (Bridge 0 0 0) . parsePipes
