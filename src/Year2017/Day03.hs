module Year2017.Day03
    ( part1
    , part2
    ) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M


midpt :: Int -> Int -> Int
midpt x y = (x + y) `div` 2

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil _ [] = []
takeUntil p (x:xs) = x : if p x then [] else takeUntil p xs

part1 :: Int -> Int
part1 n = let cornervals = scanl (+) 1 $ concatMap (replicate 2) [1..]
              (a : b : c : _) = reverse $ takeUntil (>=n) cornervals
          in b - midpt b c + abs (n - midpt a b)

type Coord = (Int, Int)
data Dir = R | U | L | D deriving (Show)

buildSpiralInts :: [Int]
buildSpiralInts = go (M.singleton (0, 0) 1) (0, 0) spiralPath
    where go :: HashMap Coord Int -> Coord -> [Dir] -> [Int]
          go _ _ [] = error "Invalid state"
          go m (x, y) (dir : rest) =
              let coord = case dir of
                        R -> (x+1, y)
                        U -> (x, y+1)
                        L -> (x-1, y)
                        D -> (x, y-1)
                  val = sumAdjacents m coord
              in val : go (M.insert coord val m) coord rest
          spiralPath = concat $ zipWith replicate (concatMap (replicate 2) [1..])
                       $ cycle [R, U, L, D]
          sumAdjacents m coord = sum $ map (\k -> M.lookupDefault 0 k m) $ adjacents coord
          adjacents (x, y) = [ (x', y')
                             | x' <- [x-1 .. x+1]
                             , y' <- [y-1 .. y+1]
                             , (x, y) /= (x', y') ]

part2 :: Int -> Int
part2 n = head $ filter (>n) buildSpiralInts
