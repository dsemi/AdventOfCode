module Year2019.Day20
    ( part1
    , part2
    ) where

import Control.Lens (_2, over)
import Data.Array
import Data.Char
import Data.Function.Memoize
import Data.List (find, transpose)
import qualified Data.Map.Strict as M
import Data.Maybe
import Linear.V2

import Utils

data Portal = Outer (V2 Int) | Inner (V2 Int) deriving (Eq, Ord, Show)
data Tile = Wall | Floor | Hole Portal | Start | End deriving (Eq, Show)

deriveMemoizable ''V2

parseMaze :: String -> (Array (V2 Int) Tile, V2 Int, V2 Int)
parseMaze (lines -> rows) = ( accumArray (flip const) Wall (V2 0 0, V2 (h-1) (w-1))
                              [ (V2 r c, get (V2 r c) v) | (r, row) <- zip [0..] rows, (c, v) <- zip [0..] row]
                            , start
                            , end )
    where get p v | M.member (Outer p) bimap = Hole (bimap M.! (Outer p))
                  | M.member (Inner p) bimap = Hole (bimap M.! (Inner p))
                  | p == start = Start
                  | p == end = End
                  | v == '.' = Floor
                  | otherwise = Wall
          portals = M.fromListWith (++) $ map (over _2 (:[])) $
                    concat (zipWith (\r row -> go (V2 r) 0 row) [0..] rows) ++
                    concat (zipWith (\c col -> go (`V2` c) 0 col) [0..] $ transpose rows)
              where go f c (a:b:'.':xs) | isUpper a && isUpper b =
                                            ([a, b], (if c == 0 then Outer else Inner) $ f (c+2)) : go f (c+3) xs
                    go f c ('.':a:b:xs) | isUpper a && isUpper b =
                                            ([a, b], (if null xs then Outer else Inner) $ f c) : go f (c+3) xs
                    go _ _ [] = []
                    go f c (_:xs) = go f (c+1) xs
          bimap = M.fromList $ concatMap (\[a, b] -> [(a, b), (b, a)]) $
                  filter ((==2) . length) $ M.elems portals
          Outer start = head $ portals M.! "AA"
          Outer end = head $ portals M.! "ZZ"
          (h, w) = (length rows, length $ head rows)

availableMoves :: Array (V2 Int) Tile -> V2 Int -> [(Int, V2 Int)]
availableMoves grid pos = case grid ! pos of
                            Hole (Outer p) -> (1, p) : moves
                            Hole (Inner p) -> (1, p) : moves
                            _ -> moves
    where moves = filter ((/= Floor) . (grid !) . snd) $ bfs pos neighbs
          neighbs st
              | st /= pos && grid ! st /= Floor = []
              | otherwise = filter ((/= Wall) . (grid !)) $ map (st+) [V2 1 0, V2 (-1) 0, V2 0 1, V2 0 (-1)]

part1 :: String -> Maybe Int
part1 input = fmap fst $ find ((== end) . snd) $ dijkstra start (memoize (availableMoves grid))
    where (grid, start, end) = parseMaze input

part2 :: String -> Maybe Int
part2 input = fmap fst $ find ((== (end, 0)) . snd) $ dijkstra (start, 0 :: Int) neighbs
    where (grid, start, end) = parseMaze input
          availMoves = memoize (availableMoves grid)
          neighbs (x, depth) = mapMaybe f $ availMoves x
              where f (dist, pos) = case grid ! pos of
                                      Hole (Outer p) | p == x -> if depth == 0 then Nothing
                                                                 else Just (dist, (pos, depth - 1))
                                      Hole (Inner p) | p == x -> Just (dist, (pos, depth + 1))
                                      _ -> Just (dist, (pos, depth))
