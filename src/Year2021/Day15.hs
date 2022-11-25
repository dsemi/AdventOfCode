module Year2021.Day15
    ( part1
    , part2
    ) where

import Control.Lens ((^.))
import Control.Monad
import Control.Monad.Extra
import Control.Monad.ST
import Data.Array.Unboxed
import Data.Char
import Data.Sequence ((|>))
import qualified Data.Sequence as S
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed.Mutable as MVU
import Linear.V2

parse :: String -> UArray (V2 Int) Int
parse input = array (V2 0 0, V2 (rows-1) (cols-1))
              [ (V2 r c, digitToInt v) | (r, row) <- zip [0..] $ lines input
              , (c, v) <- zip [0..] row ]
    where rows = length $ lines input
          cols = length $ head $ lines input

dijkstra :: UArray (V2 Int) Int -> Int
dijkstra grid = runST $ do
 lkup <- MVU.replicate (dim * dim) 0
 forM_ (assocs grid) $ \(V2 r c, v) -> MVU.write lkup (dim*(r+1)+c+1) v
 q <- MV.replicate 16 S.empty
 MV.modify q (|> dim + 1) 0
 go lkup q 0
    where dim = snd (bounds grid) ^. _x + 3
          goal = dim * dim - dim - 2
          go lkup q qi =
              MV.exchange q (qi `mod` 16) S.empty >>= foldM fld Nothing >>= maybe (go lkup q (qi+1)) pure
              where fld Nothing p
                        | p == goal = pure $ Just qi
                        | otherwise = do
                      unlessM ((<1) <$> MVU.read lkup p) $ do
                        MVU.modify lkup (*(-1)) p
                        forM_ [p-1, p+1, p-dim, p+dim] $ \n -> do
                          v <- MVU.read lkup n
                          when (v >= 1) $ MV.modify q (|> n) ((qi + v) `mod` 16)
                      pure Nothing
                    fld x _ = pure x

part1 :: String -> Int
part1 = dijkstra . parse

part2 :: String -> Int
part2 input = dijkstra grid
    where expand g = array (V2 0 0, V2 (mr*5-1) (mc*5-1))
                     [ (V2 r c, v) | r <- [0..mr*5-1]
                     , c <- [0..mc*5-1]
                     , let (rd, rm) = r `divMod` mr
                           (cd, cm) = c `divMod` mc
                     , let v = (g ! V2 rm cm - 1 + rd + cd) `mod` 9 + 1]
              where V2 mr mc = snd (bounds g) + V2 1 1
          grid = expand $ parse input
