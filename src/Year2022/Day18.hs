module Year2022.Day18
    ( part1
    , part2
    ) where

import Control.Monad
import qualified Data.HashSet as S
import Data.Ix
import Data.List.Split
import Linear.V3

import Utils

adj :: V3 Int -> [V3 Int]
adj c = [ c + d | d <- [V3 1 0 0, V3 (-1) 0 0, V3 0 1 0, V3 0 (-1) 0, V3 0 0 1, V3 0 0 (-1)] ]

cubes :: String -> S.HashSet (V3 Int)
cubes = S.fromList . map f . lines
    where f line = case map read (splitOn "," line) of
                     [x, y, z] -> V3 x y z
                     _ -> error "Malformed input"

part1 :: String -> Int
part1 input = length [ () | c <- S.toList lava, a <- adj c, not (S.member a lava) ]
    where lava = cubes input

part2 :: String -> Int
part2 input = length [ () | c <- S.toList lava, a <- adj c, S.member a air ]
    where lava = cubes input
          topLeft = fmap pred $ foldl1 (liftM2 min) lava
          botRight = fmap succ $ foldl1 (liftM2 max) lava
          neighbors pos = [ p | p <- adj pos, inRange (topLeft, botRight) p && not (S.member p lava)]
          air = S.fromList $ map snd $ bfsMany [topLeft, botRight] neighbors
