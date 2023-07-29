{-# LANGUAGE OverloadedStrings #-}

module Year2021.Day22
    ( part1
    , part2
    ) where

import Control.Arrow
import Control.Monad
import Control.Monad.ST
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.IntSet as S
import Data.Vector ((!))
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

import Scanf

parseCubes :: ByteString -> [(ByteString, Int, Int, Int, Int, Int, Int)]
parseCubes = map cube . B.lines
    where cube = (,,,,,,) |. scanf [fmt|%s x=%d..%d,y=%d..%d,z=%d..%d|]

data Interval = Interval Int Int

intersects :: Interval -> Interval -> Bool
intersects (Interval lo0 hi0) (Interval lo1 hi1) = lo0 < hi1 && lo1 < hi0

intersect :: Interval -> Interval -> Interval
intersect (Interval lo0 hi0) (Interval lo1 hi1) = Interval (max lo0 lo1) (min hi0 hi1)

len :: Interval -> Int
len (Interval lo hi) = hi - lo

data Cube = Cube Interval Interval Interval

volume :: Cube -> Int
volume (Cube x y z) = len x * len y * len z

inters :: Cube -> Cube -> Bool
inters (Cube x0 y0 z0) (Cube x1 y1 z1) = intersects x0 x1 && intersects y0 y1 && intersects z0 z1

inter :: Cube -> Cube -> Cube
inter (Cube x0 y0 z0) (Cube x1 y1 z1) = Cube (intersect x0 x1) (intersect y0 y1) (intersect z0 z1)

solve :: Int -> Int -> ByteString -> Int
solve lo hi input = sum $ map (\i -> intersectVolume (cubes ! i) (bs ! i))
                    $ filter (on !) $ [0 .. length cubes - 1]
    where activeCube = Cube (Interval lo hi) (Interval lo hi) (Interval lo hi)
          (cubes, on) = (V.fromList *** V.fromList) $ unzip
                        [ (cube, w == "on" && inters cube activeCube)
                        | (w, x0, x1, y0, y1, z0, z1) <- parseCubes input
                        , let cube = Cube (Interval x0 (x1 + 1)) (Interval y0 (y1 + 1)) (Interval z0 (z1 + 1)) ]
          bs = runST $ do
            v <- MV.replicate (length cubes) S.empty
            forM_ [0 .. length cubes - 1] $ \i ->
                forM_ [0 .. i-1] $ \j ->
                    when (inters (cubes ! i) (cubes ! j)) $ MV.modify v (S.insert i) j
            V.unsafeFreeze v
          intersectVolume cube set = S.foldl' fld (volume cube) set
              where fld vol idx = let common = inter cube (cubes ! idx)
                                      int = S.intersection set (bs ! idx)
                                  in vol - intersectVolume common int

part1 :: ByteString -> Int
part1 = solve (-50) 51

part2 :: ByteString -> Int
part2 = solve minBound maxBound
