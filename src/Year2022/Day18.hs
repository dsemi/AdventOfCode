module Year2022.Day18
    ( part1
    , part2
    ) where

import Control.Monad
import Control.Monad.ST
import qualified Data.HashSet as S
import Data.List.Split
import Data.STRef
import Linear.V3

import Utils

adj :: V3 Int -> [V3 Int]
adj c = [ c + d | d <- [V3 1 0 0, V3 (-1) 0 0, V3 0 1 0, V3 0 (-1) 0, V3 0 0 1, V3 0 0 (-1)] ]

part1 :: String -> Int
part1 = go 0 S.empty . lines
    where go cnt _ [] = cnt
          go cnt space (line:ls) = go (cnt + 6 + diff) space' ls
              where c = case map read (splitOn "," line) of
                          [x, y, z] -> V3 x y z
                          _ -> error "Malformed input"
                    space' = S.insert c space
                    diff = length (filter (`S.member` space) $ adj c) * (-2)

part2 :: String -> Int
part2 input = runST $ do
  cnt <- newSTRef 0
  let neighbors pos = do
        let ns = [ (S.member pos2 space, pos2) | pos2@(V3 x y z) <- adj pos
                 , x >= minX && x <= maxX && y >= minY && y <= maxY && z >= minZ && z <= maxZ ]
        modifySTRef' cnt (+ length (filter fst ns))
        pure $ map snd $ filter (not. fst) ns
  _ <- bfsManyM [topLeft, botRight] neighbors
  readSTRef cnt
    where space = S.fromList $ map coord $ lines input
              where coord line = case map read (splitOn "," line) of
                                   [x, y, z] -> V3 x y z
                                   _ -> error "Malformed input"
          topLeft@(V3 minX minY minZ) = fmap pred $ foldl1 (liftM2 min) space
          botRight@(V3 maxX maxY maxZ) = fmap succ $ foldl1 (liftM2 max) space
