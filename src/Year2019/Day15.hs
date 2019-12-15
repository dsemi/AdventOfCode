module Year2019.Day15
    ( part1
    , part2
    ) where

import Control.Lens
import Control.Monad.Free
import Data.List (find)
import Linear.V2

import Utils
import Year2019.IntCode


search :: Free Action () -> [(Int, (Bool, V2 Int, Free Action ()))]
search actions = bfsOn (view _2) (False, V2 0 0, actions) neighbors
    where neighbors (_, pos, Free (Input f)) = do
            (i, dir) <- [(1, V2 0 1), (2, V2 0 (-1)), (3, V2 (-1) 0), (4, V2 1 0)]
            let pos' = pos + dir
            case f i of
              Free (Output 1 cont) -> [(False, pos', cont)]
              Free (Output 2 cont) -> [(True, pos', cont)]
              _ -> []
          neighbors _ = error "Expected input"

findOxygen :: String -> Maybe (Int, Free Action ())
findOxygen = fmap (\(i, (_, _, s)) -> (i, s)) . find (view _1 . snd) . search . runPure . parse

part1 :: String -> Maybe Int
part1 = fmap fst . findOxygen

part2 :: String -> Maybe Int
part2 = fmap (fst . last . search . snd) . findOxygen
