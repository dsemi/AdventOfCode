module Year2020.Day17
    ( part1
    , part2
    ) where

import Linear.V3
import Linear.V4
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S


parse :: String -> Set (V3 Int)
parse input = S.fromList [ V3 x y 0 | (y, row) <- zip [0..] $ lines input
                         , (x, v) <- zip [0..] row, v == '#' ]

countAfterSix :: (Ord (f Int), Traversable f) => Set (f Int) -> Int
countAfterSix = length . (!! 6) . iterate go
    where go ons = let m = M.fromListWith (+) [ (neighb, 1) | pos <- S.toList ons
                                              , neighb <- traverse (\x -> [x-1, x, x+1]) pos
                                              , pos /= neighb ]
                   in S.union (S.filter (\x -> M.findWithDefault 0 x m `elem` [2, 3]) ons)
                          $ S.filter (\x -> S.notMember x ons && m M.! x == 3) $ M.keysSet m

part1 :: String -> Int
part1 = countAfterSix . parse

part2 :: String -> Int
part2 = countAfterSix . S.map vector . parse
