module Year2019.Day11
    ( part1
    , part2
    ) where

import Control.Monad
import Control.Lens
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Linear.V2

import Year2019.IntCode


turn :: V2 Int -> Int -> V2 Int
turn (V2 x y) 0 = V2 y (-x)
turn (V2 x y) 1 = V2 (-y) x
turn _ _ = error "Unknown dir"

runRobot :: Map (V2 Int) Int -> Memory -> Map (V2 Int) Int
runRobot d prog = points
    where (ins, points) = go d (V2 0 0) (V2 0 (-1)) outs
          outs = runWithInput ins prog
          go m pos dir xss =
              over _1 (M.findWithDefault 0 pos m :)
                 $ case xss of
                     (col : (turn dir -> dir') : xs) ->
                         go (M.insert pos col m) (pos + dir') dir' xs
                     [] -> ([], m)
                     _ -> error "Invalid number of outputs"

part1 :: String -> Int
part1 = M.size . runRobot M.empty . parse

draw :: Map (V2 Int) Int -> String
draw points = ('\n' :) . init . unlines
              $ flip map [minY .. maxY] $ \y ->
                  flip map [minX .. maxX] $ \x ->
                      case M.findWithDefault 0 (V2 x y) points of
                        0 -> ' '
                        _ -> '#'
    where V2 minX minY = foldr1 (liftM2 min) $ M.keys points
          V2 maxX maxY = foldr1 (liftM2 max) $ M.keys points

part2 :: String -> String
part2 = draw . runRobot (M.singleton (V2 0 0) 1) . parse
