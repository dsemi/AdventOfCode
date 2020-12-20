module Year2020.Day20
    ( part1
    , part2
    ) where

import Control.Lens
import Data.Array.Unboxed
import Data.List (delete)
import Data.List.Split
import qualified Data.Map as M
import qualified Data.Set as S
import Linear.V2


data Tile = Tile { _num :: Int
                 , _shape :: UArray (V2 Int) Char
                 }
makeLenses ''Tile

instance Eq Tile where
    (Tile n1 _) == (Tile n2 _) = n1 == n2

parse :: String -> [Tile]
parse = map parseTile . splitOn "\n\n"
    where parseTile tile = let (t, grid) = splitAt 1 $ lines tile
                               arr = [ (V2 r c, v) | (r, row) <- zip [0..] grid
                                     , (c, v) <- zip [0..] row]
                               min' = minimum $ map fst arr
                               max' = maximum $ map fst arr
                           in Tile (read $ init $ last $ words $ head t) $ array (min', max') arr

orientations :: UArray (V2 Int) Char -> [UArray (V2 Int) Char]
orientations arr = [ arr
                   , ixmap (bounds arr) (\(V2 r c) -> V2 r (m-c)) arr
                   , ixmap (bounds arr) (\(V2 r c) -> V2 (m-r) c) arr
                   , ixmap (bounds arr) (\(V2 r c) -> V2 (m-r) (m-c)) arr
                   , ixmap (bounds arr) (\(V2 r c) -> V2 c r) arr
                   , ixmap (bounds arr) (\(V2 r c) -> V2 c (m-r)) arr
                   , ixmap (bounds arr) (\(V2 r c) -> V2 (m-c) r) arr
                   , ixmap (bounds arr) (\(V2 r c) -> V2 (m-c) (m-r)) arr
                   ]
    where (V2 _ m) = snd $ bounds arr

placeTiles :: [Tile] -> M.Map (V2 Int) Tile
placeTiles tiles = head $ go 0 M.empty tiles
    where len = floor $ sqrt $ fromIntegral $ length tiles
          (V2 mr mc) = snd $ bounds $ _shape $ head tiles
          go _ m [] = [m]
          go c m ts = [ m' | tile <- ts
                      , t <- traverseOf shape orientations tile
                      , let coord = V2 (c `div` len) (c `mod` len)
                      -- Either in first col or verify that the right col of the left tile matches
                      -- the left col of this tile
                      , c `mod` len == 0 || (let (Tile _ lef) = m M.! (coord - V2 0 1)
                                                 (Tile _ rig) = t
                                             in all (\i -> lef ! V2 i mc == rig ! V2 i 0) [0..mr])
                      -- Either in first row or verify that the bottom row of the above tile matches
                      -- the top row of this tile
                      , c < len || (let (Tile _ top) = m M.! (coord - V2 1 0)
                                        (Tile _ bot) = t
                                    in all (\i -> top ! V2 mr i == bot ! V2 0 i) [0..mc])
                      , m' <- go (c+1) (M.insert coord t m) $ delete t ts ]

part1 :: String -> Int
part1 input = let m = placeTiles $ parse input
                  (V2 r c) = maximum $ M.keys m
              in product $ map (view num . (m M.!)) [V2 0 0, V2 0 c, V2 r 0, V2 r c]


-- Consolidate map of tiles into one large tile, while skipping borders of inner tiles
image :: M.Map (V2 Int) Tile -> UArray (V2 Int) Char
image m = array (V2 0 0, V2 ((mr+1)*mr' - 1) ((mc+1)*mc' - 1)) grid
    where (V2 mr mc) = maximum $ M.keys m
          (V2 mr' mc') = fmap pred $ snd $ bounds $ view shape $ head $ M.elems m
          grid = [ (V2 r c, _shape (m M.! (V2 r' c')) ! (V2 r'' c'')) | r <- [0..((mr+1)*mr' - 1)]
                 , c <- [0..((mc+1)*mc' - 1)]
                 , let r' = r `div` mr'
                 , let r'' = r `mod` mr' + 1
                 , let c' = c `div` mc'
                 , let c'' = c `mod` mc' + 1 ]

seaMonster :: S.Set (V2 Int)
seaMonster = S.fromList [ V2 r c | (r, row) <- zip [0..] str
                        , (c, v) <- zip [0..] row
                        , v == '#']
    where str = [ "                  # "
                , "#    ##    ##    ###"
                , " #  #  #  #  #  #   " ]

-- Assumes that sea monsters do not overlap
findSeaMonsters :: UArray (V2 Int) Char -> Int
findSeaMonsters p = head [ sum' | arr <- orientations p
                         , let sum' = sum [ S.size seaMonster | r <- [0..(mr - mrp)]
                                          , c <- [0..(mc - mcp)]
                                          , all ((=='#') . (arr !)) $ S.map (+V2 r c) seaMonster ]
                         , sum' /= 0]
    where (V2 mr mc) = snd $ bounds p
          mrp = maximum $ S.map (^. _x) seaMonster
          mcp = maximum $ S.map (^. _y) seaMonster

part2 :: String -> Int
part2 input = let m = placeTiles $ parse input
                  p = image m
                  n = findSeaMonsters p
              in length (filter (=='#') $ elems p) - n
