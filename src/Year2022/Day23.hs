{-# LANGUAGE FlexibleContexts #-}

module Year2022.Day23
    ( part1
    , part2
    ) where

import Control.Monad
import Control.Monad.ST
import Data.Bits
import Data.List (foldl')
import Data.STRef
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

import Utils (windows)

type Grid = Vector Integer

data Dir = North | South | West | East

stepWest :: Integer -> Integer
stepWest row = shiftR row 1

stepEast :: Integer -> Integer
stepEast row = shiftL row 1

makeGrid :: String -> Grid
makeGrid input = runST $ do
  res <- MV.replicate 160 0
  forM_ (zip [24..] $ lines input) $ \(r, line) -> do
    MV.write res r $ foldl' (\acc (c, v) -> acc .|. (shiftL (if v == '#' then 1 else 0) c)) 0 $ zip [72..] line
  V.freeze res

propose :: [Dir] -> [Integer] -> [Integer] -> [Integer] -> [Integer]
propose dirs [nw, n, ne] [w, c, e] [sw, s, se] =
    let proposals = V.replicate 4 c
        passed = nw .|. n .|. ne .|. w .|. e .|. sw .|. s .|. se
    in V.toList $ fst $ foldl' go (proposals, passed) $ take 4 dirs
    where go (props, pass) d = let (i, dir) = case d of
                                                North -> (0, complement (ne .|. n .|. nw))
                                                South -> (1, complement (se .|. s .|. sw))
                                                West -> (2, complement (nw .|. w .|. sw))
                                                East -> (3, complement (ne .|. e .|. se))
                               in ( props V.// [(i, props V.! i .&. dir .&. pass)]
                                  , pass .&. complement dir )

checkCollisions :: [Integer] -> [Integer] -> [Integer] -> [Integer]
checkCollisions [_, s, _, _] [_, _, w, e] [n, _, _, _] =
    [ n .&. complement s
    , s .&. complement n
    , stepWest w .&. complement (stepEast e)
    , stepEast e .&. complement (stepWest w) ]

zeroes :: [Integer]
zeroes = [0, 0]

step dirs grid = do
  moved <- newSTRef False
  frz <- V.freeze grid
  let froms = map (\[above, cur, below] -> checkCollisions above cur below) $ windows 3 $
              map (\[above, cur, below] -> propose dirs above cur below) $ windows 3 $
              map (\row -> [stepEast row, row, stepWest row]) $ zeroes ++ V.toList frz ++ zeroes
  forM_ (zip [0..] froms) $ \(i, [fromS, fromN, fromE, fromW]) ->
      let dests = fromN .|. fromS .|. fromW .|. fromE
      in when (dests /= 0) $ do
           writeSTRef moved True
           MV.modify grid (.&. complement fromS) (i + 1)
           MV.modify grid (.&. complement fromN) (i - 1)
           MV.modify grid (.&. (complement (stepWest fromW) .&. complement (stepEast fromE))) i
           MV.modify grid (.|. dests) i
  readSTRef moved

part1 :: String -> Int
part1 input = runST $ do
  grid <- V.thaw $ makeGrid input
  go 10 dirs grid
  (x0, x1, y0, y1, cnt) <- V.ifoldl' f (9999, 0, 9999, 0, 0) <$> V.freeze grid
  pure $ (x1 - x0) * (y1 - y0) - cnt
    where dirs = cycle [North, South, West, East]
          go 0 _ _ = pure ()
          go c ds grd = do
            void $ step ds grd
            go (c-1) (tail ds) grd
          f (x0, x1, y0, y1, cnt) _ 0 = (x0, x1, y0, y1, cnt)
          f (x0, x1, y0, y1, cnt) r n = ( min x0 $ length $ takeWhile (not . (testBit n)) $ [0..]
                                        , max x1 $ length $ takeWhile (/= 0) $ iterate (`shiftR` 1) n
                                        , min y0 r
                                        , max y1 (r+1)
                                        , cnt + popCount n )

part2 :: String -> Int
part2 input = runST $ do
  grid <- V.thaw $ makeGrid input
  go 1 dirs grid
    where dirs = cycle [North, South, West, East]
          go i ds grd = do
            b <- step ds grd
            if b then go (i+1) (tail ds) grd
            else pure i
