module Year2019.Day24
    ( part1
    , part2
    ) where

import Data.Bool
import qualified Data.IntSet as S
import Graphics.Image (Border(..), Image, Pixel, VU, X, convolve, fromLists, toLists)
import qualified Graphics.Image as G


type Planet = Image VU X Int

parseGrid :: String -> Planet
parseGrid = fromLists . map (map (bool 0 1 . (=='#'))) . lines

neighborCount :: Planet -> Planet
neighborCount = convolve (Fill 0) (fromLists [ [0, 1, 0], [1, 0, 1], [0, 1, 0] ])

nextBug :: Pixel X Int -> Pixel X Int -> Pixel X Int
nextBug 1 adjBugs = bool 0 1 $ adjBugs == 1
nextBug 0 adjBugs = bool 0 1 $ adjBugs == 1 || adjBugs == 2
nextBug d _ = error $ "Square is defined as bug (1) or space (0), not: " ++ show d

findDupBio :: Planet -> Int
findDupBio = go S.empty
    where go s grid | S.member bio s = bio
                    | otherwise = go (S.insert bio s) $ G.zipWith nextBug grid $ neighborCount grid
              where bio = sum $ zipWith ((G.getX .) . (*)) (map (2^) [0..]) $ concat $ toLists grid

part1 :: String -> Int
part1 = findDupBio . parseGrid

empty :: Planet
empty = fromLists $ replicate 5 (replicate 5 0)

pad :: [Planet] -> [Planet]
pad planets = [empty, empty] ++ planets ++ [empty, empty]

step :: [Planet] -> [Planet]
step (pad -> planets) = zipWith3 (\o v i -> G.izipWith (f o i) v $ neighborCount v)
                        planets (tail planets) (tail (tail planets))
    where f :: Planet -> Planet -> (Int, Int) -> Pixel X Int -> Pixel X Int -> Pixel X Int
          f _          _          (2, 2) _ _ = 0
          f outerLevel innerLevel (r, c) b bugsAtCurrentLevel = nextBug b numBugs
              where numBugs = sum $ concat
                      [ [ bugsAtCurrentLevel ]
                      , [ G.index outerLevel (1, 2) | r == 0 ]
                      , [ G.index outerLevel (3, 2) | r == 4 ]
                      , [ G.index outerLevel (2, 1) | c == 0 ]
                      , [ G.index outerLevel (2, 3) | c == 4 ]
                      , [ G.index innerLevel (0, x) | (r, c) == (1, 2), x <- [0..4] ]
                      , [ G.index innerLevel (4, x) | (r, c) == (3, 2), x <- [0..4] ]
                      , [ G.index innerLevel (y, 0) | (r, c) == (2, 1), y <- [0..4] ]
                      , [ G.index innerLevel (y, 4) | (r, c) == (2, 3), y <- [0..4] ] ]

part2 :: String -> Int
part2 = sum . map countBugs . (!! 200) . iterate step . (:[]) . parseGrid
    where countBugs = G.getX . sum . concat . toLists
