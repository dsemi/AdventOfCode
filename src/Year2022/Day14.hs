module Year2022.Day14
    ( part1
    , part2
    ) where

import Control.Monad.ST
import Data.Array.Base
import Data.Array.Unboxed
import Data.List.Split
import Linear.V2

parseGrid :: String -> UArray (V2 Int) Char
parseGrid input = accumArray (flip const) '.' (V2 0 0, V2 1000 (maxY + 2))
                  [ (pt, '#') | pts <- ptss
                  , pt <- concat $ zipWith (\a b -> range (min a b, max a b)) pts (tail pts) ]
    where coord pt = let [x, y] = map read $ splitOn "," pt in V2 x y
          ptss = map (map coord . splitOn " -> ") $ lines input
          maxY = maximum $ map (\(V2 _ y) -> y) $ concat ptss

flowSand :: Bool -> UArray (V2 Int) Char -> Int
flowSand p2 g = runST $ do
  grid <- thaw g
  go grid $ V2 500 0
  length . filter (== 'o') <$> getElems grid
    where (V2 _ maxY) = snd $ bounds g
          go :: STUArray s (V2 Int) Char -> V2 Int -> ST s Bool
          go grid pt@(V2 _ y)
              | y >= maxY = pure p2
              | otherwise = readArray grid pt >>=
                            \case
                            '~' -> pure False
                            '#' -> pure True
                            'o' -> pure True
                            _ -> do
                              b <- go grid (pt + V2 0 1)
                              b' <- if b then go grid (pt + V2 (-1) 1) else pure b
                              b'' <- if b' then go grid (pt + V2 1 1) else pure b'
                              writeArray grid pt $ if b'' then 'o' else '~'
                              pure b''

part1 :: String -> Int
part1 = flowSand False . parseGrid

part2 :: String -> Int
part2 = flowSand True . parseGrid
