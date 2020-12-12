module Year2020.Day12
    ( part1
    , part2
    ) where

import Control.Lens
import Data.List (foldl')
import Linear.V2


data Action = N Int | S Int | E Int | W Int
            | L Int | R Int | F Int

-- Assumes L and R are multiples of 90 degrees
mkAction :: String -> Action
mkAction ('N':rest) = N $ read rest
mkAction ('S':rest) = S $ read rest
mkAction ('E':rest) = E $ read rest
mkAction ('W':rest) = W $ read rest
mkAction ('L':rest) = L $ read rest `div` 90
mkAction ('R':rest) = R $ read rest `div` 90
mkAction ('F':rest) = F $ read rest
mkAction _ = error "bad input"

travel :: V2 Int -> Bool -> String -> Int
travel start moveShip = sum . abs . fst . foldl' (flip go) (V2 0 0, start) . map mkAction . lines
    where go = \case
               N n -> over (if moveShip then _1 else _2) (+ V2 n n * V2 0 1)
               S n -> over (if moveShip then _1 else _2) (+ V2 n n * V2 0 (-1))
               E n -> over (if moveShip then _1 else _2) (+ V2 n n * V2 1 0)
               W n -> over (if moveShip then _1 else _2) (+ V2 n n * V2 (-1) 0)
               L n -> over _2 ((!! n) . iterate (\(V2 x y) -> V2 (-y) x))
               R n -> over _2 ((!! n) . iterate (\(V2 x y) -> V2 y (-x)))
               F n -> \(pos, dir) -> (pos + V2 n n * dir, dir)

part1 :: String -> Int
part1 = travel (V2 1 0) True

part2 :: String -> Int
part2 = travel (V2 10 1) False
