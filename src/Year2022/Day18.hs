module Year2022.Day18
    ( part1
    , part2
    ) where

import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.HashSet as S
import Data.Ix
import Linear.V3

import Scanf
import Utils

adj :: V3 Int -> [V3 Int]
adj c = [ c + d | d <- [V3 1 0 0, V3 (-1) 0 0, V3 0 1 0, V3 0 (-1) 0, V3 0 0 1, V3 0 0 (-1)] ]

cubes :: ByteString -> S.HashSet (V3 Int)
cubes = S.fromList . map (apply V3 . scanf [fmt|%d,%d,%d|]) . B.lines

part1 :: ByteString -> Int
part1 input = length [ () | c <- S.toList lava, a <- adj c, not (S.member a lava) ]
    where lava = cubes input

part2 :: ByteString -> Int
part2 input = length [ () | c <- S.toList lava, a <- adj c, S.member a air ]
    where lava = cubes input
          topLeft = fmap pred $ foldl1 (liftM2 min) lava
          botRight = fmap succ $ foldl1 (liftM2 max) lava
          neighbors pos = [ p | p <- adj pos, inRange (topLeft, botRight) p && not (S.member p lava)]
          air = S.fromList $ map snd $ bfsMany [topLeft, botRight] neighbors
