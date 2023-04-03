module Year2018.Day25
    ( part1
    , part2
    ) where

import Control.Monad
import Control.Monad.ST
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.IntSet as S
import FlatParse.Basic
import Linear.V4
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

import Utils

data Node = Node { pt :: V4 Int
                 , parent :: Int
                 , rank :: Int }

parsePoints :: ByteString -> Vector Node
parsePoints = V.fromList . zipWith (\i l -> (\p -> Node p i 0) $ parse l) [0..] . B.lines
    where parser = V4 <$> signedInt <* $(char ',') <*> signedInt <* $(char ',') <*>
                   signedInt <* $(char ',') <*> signedInt
          parse line = case runParser parser line of
                         OK res _ -> res
                         _ -> error "unreachable"

find :: MV.STVector s Node -> Int -> ST s Int
find v = go
    where go k = do
            p <- parent <$> MV.unsafeRead v k
            if k == p then pure k
            else go p

union :: MV.STVector s Node -> Int -> Int -> ST s ()
union v x y = do
  xRoot <- find v x
  yRoot <- find v y
  when (xRoot /= yRoot) $ do
    xr <- rank <$> MV.unsafeRead v xRoot
    yr <- rank <$> MV.unsafeRead v yRoot
    case compare xr yr of
      LT -> MV.unsafeModify v (\n -> n { parent = yRoot }) xRoot
      GT -> MV.unsafeModify v (\n -> n { parent = xRoot }) yRoot
      EQ -> do
        MV.unsafeModify v (\n -> n { parent = xRoot }) yRoot
        MV.unsafeModify v (\n -> n { rank = rank n + 1 }) xRoot

dist :: V4 Int -> V4 Int -> Int
dist (V4 w0 x0 y0 z0) (V4 w1 x1 y1 z1) =
    abs (w0 - w1) + abs (x0 - x1) + abs (y0 - y1) + abs (z0 - z1)

constellations :: Vector Node -> Int
constellations pts = runST $ do
  v <- V.thaw pts
  forM_ [0 .. lst] $ \i ->
      forM_ [i + 1 .. lst] $ \j ->
          when (dist (pt $ pts ! i) (pt $ pts ! j) <= 3) $
               union v i j
  S.size . S.fromList <$> mapM (find v) [0 .. lst]
    where lst = V.length pts - 1

part1 :: ByteString -> Int
part1 = constellations . parsePoints

part2 :: ByteString -> String
part2 = const " "
