module Year2020.Day20
    ( part1
    , part2
    ) where

import Control.Lens
import Data.Array.Unboxed
import Data.List (delete, nub, partition)
import Data.List.Split
import Data.Map (Map)
import qualified Data.Map as M
import Linear.V2


data Tile = Tile { _num :: Int
                 , _shape :: UArray (V2 Int) Bool
                 }
makeLenses ''Tile

instance Eq Tile where
    (Tile n1 _) == (Tile n2 _) = n1 == n2

parse :: String -> [Tile]
parse = map parseTile . splitOn "\n\n"
    where parseTile tile = let (t, grid) = splitAt 1 $ lines tile
                               arr = [ (V2 r c, v == '#') | (r, row) <- zip [0..] grid
                                     , (c, v) <- zip [0..] row]
                               min' = minimum $ map fst arr
                               max' = maximum $ map fst arr
                           in Tile (read $ init $ last $ words $ head t) $ array (min', max') arr

orientations :: UArray (V2 Int) Bool -> [UArray (V2 Int) Bool]
orientations arr = [ arr
                   , ixmap bds (\(V2 r c) -> V2 r (m-c)) arr
                   , ixmap bds (\(V2 r c) -> V2 (m-r) c) arr
                   , ixmap bds (\(V2 r c) -> V2 (m-r) (m-c)) arr
                   , ixmap bds (\(V2 r c) -> V2 c r) arr
                   , ixmap bds (\(V2 r c) -> V2 c (m-r)) arr
                   , ixmap bds (\(V2 r c) -> V2 (m-c) r) arr
                   , ixmap bds (\(V2 r c) -> V2 (m-c) (m-r)) arr
                   ]
    where bds@(_, V2 _ m) = bounds arr

findCorners :: [Tile] -> [Int]
findCorners tiles = case map fst (filter ((==2) . snd) (M.toList uniques)) of
                      ns | length ns == 4 -> ns
                      ns -> error $ "Couldn't find all corners: " ++ show ns
    where m = M.fromListWith (++) $ concatMap hashSides tiles
          uniques = M.fromListWith (+) $ map ((,1) . head) $ filter ((==1) . length) $ M.elems m
          hashSides (Tile n tile) = map (,[n]) [ hashSide $ map ((tile !) . (V2 0)) [0..mc]
                                               , hashSide $ map ((tile !) . (V2 mr)) [0..mc]
                                               , hashSide $ map ((tile !) . (`V2` 0)) [0..mr]
                                               , hashSide $ map ((tile !) . (`V2` mc)) [0..mr] ]
              where (V2 mr mc) = snd $ bounds tile
          hashSide :: [Bool] -> Int
          hashSide ds = min (foldl (\acc x -> 2*acc + fromEnum x) 0 ds)
                        $ foldl (\acc x -> 2*acc + fromEnum x) 0 $ reverse ds

placeTiles :: String -> Map (V2 Int) Tile
placeTiles input = head $ go 0 M.empty t1 t2
    where tiles = parse input
          len = floor $ sqrt $ fromIntegral $ length tiles
          (V2 mr mc) = snd $ bounds $ _shape $ head tiles
          corners = findCorners tiles
          (t1, t2) = partition ((`elem` corners) . _num) tiles
          go _ m [] [] = [m]
          go c m ta tb = [ m' | let isCorner = c `elem` [0, len-1, len*(len-1), len*len-1]
                         , tile <- if isCorner then ta else tb
                         , t <- traverseOf shape orientations tile
                         , let coord@(V2 row col) = V2 (c `div` len) (c `mod` len)
                         , col == 0 || (let (Tile _ lef) = m M.! (coord - V2 0 1)
                                            (Tile _ rig) = t
                                        in all (\i -> lef ! V2 i mc == rig ! V2 i 0) [0..mr])
                         , row == 0 || (let (Tile _ top) = m M.! (coord - V2 1 0)
                                            (Tile _ bot) = t
                                        in all (\i -> top ! V2 mr i == bot ! V2 0 i) [0..mc])
                         , m' <- go (c+1) (M.insert coord t m) (delete t ta) $ delete t tb ]

part1 :: String -> Int
part1 input = let m = placeTiles input
                  (V2 r c) = maximum $ M.keys m
              in product $ map (view num . (m M.!)) [V2 0 0, V2 0 c, V2 r 0, V2 r c]

image :: Map (V2 Int) Tile -> UArray (V2 Int) Bool
image m = array (V2 0 0, V2 ((mr+1)*mr' - 1) ((mc+1)*mc' - 1)) grid
    where (V2 mr mc) = maximum $ M.keys m
          (V2 mr' mc') = fmap pred $ snd $ bounds $ view shape $ head $ M.elems m
          grid = [ (V2 r c, _shape (m M.! (V2 r' c')) ! (V2 r'' c'')) | r <- [0..((mr+1)*mr' - 1)]
                 , c <- [0..((mc+1)*mc' - 1)]
                 , let r' = r `div` mr'
                 , let r'' = r `mod` mr' + 1
                 , let c' = c `div` mc'
                 , let c'' = c `mod` mc' + 1 ]

seaMonster :: [V2 Int]
seaMonster = [ V2 r c | (r, row) <- zip [0..] str
             , (c, v) <- zip [0..] row
             , v == '#']
    where str = [ "                  # "
                , "#    ##    ##    ###"
                , " #  #  #  #  #  #   " ]

findSeaMonsters :: UArray (V2 Int) Bool -> Int
findSeaMonsters p = head [ length pts | arr <- orientations p
                         , let pts = nub [ pos | r <- [0..(mr - mrp)]
                                         , c <- [0..(mc - mcp)]
                                         , let newSeaMonster = map (+V2 r c) seaMonster
                                         , all (arr !) newSeaMonster
                                         , pos <- map (+V2 r c) seaMonster ]
                         , not $ null pts ]
    where (V2 mr mc) = snd $ bounds p
          mrp = maximum $ map (^. _x) seaMonster
          mcp = maximum $ map (^. _y) seaMonster

part2 :: String -> Int
part2 input = let m = placeTiles input
                  p = image m
                  n = findSeaMonsters p
              in length (filter id $ elems p) - n
