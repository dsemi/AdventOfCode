module Year2021.Day05
    ( part1
    , part2
    ) where

import Control.Monad
import Data.Array.MArray;
import Data.Array.ST;
import Data.Array.Unboxed;
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Linear.V2

import Scanf

solve :: Bool -> ByteString -> Int
solve p2 input = length $ filter (>1) $ elems grid
    where parse = (\a b c d -> (V2 a b, V2 c d)) |. scanf [fmt|%d,%d -> %d,%d|]
          lns = map parse $ B.lines input
          maxX = maximum $ map (\(V2 x0 _, V2 x1 _) -> max x0 x1) lns
          maxY = maximum $ map (\(V2 _ y0, V2 _ y1) -> max y0 y1) lns
          grid :: UArray (V2 Int) Int
          grid = runSTUArray $ do
            arr <- newArray (V2 0 0, V2 maxX maxY) 0
            let validLines = filter (\(V2 x0 y0, V2 x1 y1) -> p2 || x0 == x1 || y0 == y1) lns
            forM_ validLines $ \(c, c1) -> do
              let d = signum $ c1 - c
                  pts = takeWhile (/= c1 + d) $ scanl (+) c $ repeat d
              forM_ pts $ \p ->
                  readArray arr p >>= writeArray arr p . (+1)
            pure arr

part1 :: ByteString -> Int
part1 = solve False

part2 :: ByteString -> Int
part2 = solve True
