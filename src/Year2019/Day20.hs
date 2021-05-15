module Year2019.Day20
    ( part1
    , part2
    ) where

import Control.Lens
import Data.Array.Unboxed
import Data.Char
import Data.Either
import Data.List (find, tails, transpose)
import Data.Maybe
import qualified Data.Map.Strict as M
import Linear.V2

import Utils


parseMaze :: String -> (V2 Int, V2 Int, (V2 Int, Int) -> [(V2 Int, Int)])
parseMaze (lines -> rows) = (invert outer M.! "AA", invert outer M.! "ZZ", neighbors)
    where grid :: UArray (V2 Int) Bool
          grid = accumArray (flip const) False (V2 0 0, V2 (length (head rows) - 1) (length rows - 1))
                 [ (V2 x y, v == '.') | (y, row) <- zip [0..] rows, (x, v) <- zip [0..] row ]
          match f = catMaybes . zipWith go [0..] . tails
              where go n (a:b:'.':_) | isUpper a && isUpper b = Just $ f (n+2) [a, b]
                    go n ('.':a:b:_) | isUpper a && isUpper b = Just $ f n [a, b]
                    go _ _ = Nothing
          makePortal len f p s | p == 2 || p == len - 3 = Left (f p, s)
                               | otherwise = Right (f p, s)
          portals = concat (zipWith (fun $ flip V2) [0..] rows)
                    ++ concat (zipWith (fun V2) [0..] (transpose rows))
              where fun f p ps = match (makePortal (length ps) (f p)) ps
          (outer, inner) = over both M.fromList $ partitionEithers portals
          invert = M.fromList . map (\(a, b) -> (b, a)) . M.toList
          outer' = M.mapMaybe (`M.lookup` invert inner) outer
          inner' = M.mapMaybe (`M.lookup` invert outer) inner
          adjs p = filter (grid !) $ map (p+) [V2 1 0, V2 (-1) 0, V2 0 1, V2 0 (-1)]
          neighbors (pos, level) = catMaybes $ ((,level-1) <$> M.lookup pos outer') :
                                               ((,level+1) <$> M.lookup pos inner') :
                                               map (Just . (,level)) (adjs pos)

part1 :: String -> Maybe Int
part1 = search . parseMaze
    where search (start, end, neighbors) = fmap fst $ find ((==end) . snd)
                                           $ bfs start $ map fst . neighbors . (,undefined)

part2 :: String -> Maybe Int
part2 = search . parseMaze
    where search (start, end, neighbors) = fmap fst $ find ((==(end, 0)) . snd)
                                           $ bfs (start, 0) $ filter ((>=0) . snd) . neighbors
