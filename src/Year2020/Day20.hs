{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Year2020.Day20
    ( part1
    , part2
    ) where

import Data.List (partition, tails, transpose)
import Data.List.Split
import qualified Data.Map as M


type Tile = (Int, [[Bool]])

parse :: String -> [Tile]
parse = map parseTile . splitOn "\n\n"
    where parseTile tile = let (t:grid) = lines tile
                           in (read $ init $ last $ words t, map (map (=='#')) grid)

rotFlip :: Bool -> Int -> [[a]] -> [[a]]
rotFlip False n = (!! n) . iterate (transpose . reverse)
rotFlip True  n = (!! n) . iterate (transpose . reverse) . reverse

orientations :: [[a]] -> [[[a]]]
orientations arr = [ rotFlip f n arr | f <- [False, True], n <- [0..3] ]

findCorners :: [Tile] -> [Int]
findCorners tiles = case map fst (filter ((==2) . snd) (M.toList uniques)) of
                      ns | length ns == 4 -> ns
                      ns -> error $ "Couldn't find all corners: " ++ show ns
    where m = M.fromListWith (++) $ concatMap hashSides tiles
          uniques = M.fromListWith (+) $ map ((,1) . head) $ filter ((==1) . length) $ M.elems m
          hashSides (n, tile) = [ (hashSide (head (rotFlip False x tile)), [n]) | x <- [0..3] ]
          hashSide ds = min (foldl (\acc x -> 2*acc + fromEnum x) 0 ds)
                        $ foldl (\acc x -> 2*acc + fromEnum x) 0 $ reverse ds

part1 :: String -> Int
part1 = product . findCorners . parse

placeTiles :: String -> [[Bool]]
placeTiles input = foldr1 (++) $ map (foldr1 (zipWith (++))) $ chunksOf len $ map crop placedTiles
    where crop = map (init . tail) . init . tail
          tiles = parse input
          placedTiles = map (snd . snd) $ M.toAscList $ head $ go 0 M.empty t1 t2
          len = floor $ sqrt $ fromIntegral $ length tiles
          corners = findCorners tiles
          (t1, t2) = partition ((`elem` corners) . fst) tiles
          go _ m [] [] = [m]
          go c m ta tb = [ m' | let isCorner = c `elem` [0, len-1, len*(len-1), len*len-1]
                         , (n, tile) <- if isCorner then ta else tb
                         , t <- orientations tile
                         , c `mod` len == 0 || and (zipWith (==) (map head t) $ map last
                                                   $ snd $ m M.! (c - 1))
                         , c < len || and (zipWith (==) (head t) $ last $ snd $ m M.! (c - len))
                         , m' <- go (c+1) (M.insert c (n, t) m)
                                 (filter ((/=n) . fst) ta)
                                 (filter ((/=n) . fst) tb) ]

seaMonster :: [[Bool]]
seaMonster = map (map (=='#')) [ "                  # "
                               , "#    ##    ##    ###"
                               , " #  #  #  #  #  #   " ]

findSeaMonsters :: [[Bool]] -> Int
findSeaMonsters = head . filter (>0) . map count . orientations
    where h = length seaMonster
          w = length $ head seaMonster
          nOn = length $ filter id $ concat seaMonster
          count arr = sum [ nOn | x <- takeWhile ((>= w) . length) $ tails arr
                          , y <- takeWhile ((>= h) . length) $ tails $ transpose x
                          , and $ zipWith ((and .) . zipWith (\a b -> not b || a)) y seaMonster ]

part2 :: String -> Int
part2 input = let p = placeTiles input
                  n = findSeaMonsters p
              in length (filter id $ concat p) - n
