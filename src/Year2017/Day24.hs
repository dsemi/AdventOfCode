module Year2017.Day24
    ( part1
    , part2
    ) where

import Control.Arrow
import Data.List (delete)
import Data.List.Split


type Pipe = (Int, Int)
type Bridge = (Int, [Pipe])

connect :: Bridge -> Pipe -> Maybe Bridge
connect (pins, pipes) pipe@(a, b)
    | pins == a = Just (b, pipe : pipes)
    | pins == b = Just (a, pipe : pipes)
    | otherwise = Nothing

parsePipes :: String -> [Pipe]
parsePipes = map parse . lines
    where parse = f . splitOn "/"
          f [x, y] = (read x, read y)
          f _ = error "Invalid pipe"

combos :: [Pipe] -> [[Pipe]]
combos = go (0, [])
    where go br [] = [ snd br ]
          go br ps = [ x | pipe <- ps
                     , Just br' <- [connect br pipe]
                     , x <- snd br' : go br' (delete pipe ps) ]

strength :: [Pipe] -> Int
strength = sum . map (uncurry (+))

part1 :: String -> Int
part1 = maximum . map strength . combos . parsePipes

part2 :: String -> Int
part2 = snd . maximum . map (length &&& strength) . combos . parsePipes
