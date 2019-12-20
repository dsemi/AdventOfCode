{-# LANGUAGE NamedFieldPuns #-}

module Year2019.Day20
    ( part1
    , part2
    ) where

import Control.Lens
import Data.Char
import Data.Either
import Data.List (elemIndices, tails, transpose)
import Data.Maybe
import Data.Map.Strict ((!))
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Linear.V2

import DaysTH (UnalteredString(..))
import Utils


data Maze = Maze { start :: V2 Int
                 , end :: V2 Int
                 , neighbors :: (V2 Int, Int) -> [(V2 Int, Int)]
                 }

parseMaze :: String -> Maze
parseMaze (lines -> rows) = Maze (invert outer ! "AA") (invert outer ! "ZZ") neighbors
    where maze = S.fromList [V2 x y | (y, row) <- zip [0..] rows, x <- elemIndices '.' row]
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
          adjs p = filter (`S.member` maze) $ map (p+) [V2 1 0, V2 (-1) 0, V2 0 1, V2 0 (-1)]
          neighbors (pos, level) = catMaybes $ ((,level-1) <$> M.lookup pos outer') :
                                               ((,level+1) <$> M.lookup pos inner') :
                                               map (Just . (,level)) (adjs pos)

part1 :: UnalteredString -> Int
part1 = search . parseMaze . unwrap
    where search Maze{start, end, neighbors} = fst $ head $ filter ((==end) . snd)
                                               $ bfs start $ map fst . neighbors . (,undefined)

part2 :: UnalteredString -> Int
part2 = search . parseMaze . unwrap
    where search Maze{start, end, neighbors} = fst $ head $ filter ((==(end, 0)) . snd)
                                               $ bfs (start, 0) $ filter ((>=0) . snd) . neighbors
