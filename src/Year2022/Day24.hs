{-# LANGUAGE RecordWildCards #-}

module Year2022.Day24
    ( part1
    , part2
    ) where

import Control.Monad
import Control.Monad.ST
import Control.Monad.State.Strict
import Data.Bits
import Data.List (transpose)
import Data.Vector.Unboxed ((!), Vector)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Word

data Valley a = Valley { w :: Int
                       , h :: Int
                       , nBlizz :: a
                       , sBlizz :: a
                       , wBlizz :: a
                       , eBlizz :: a
                       , walls :: Vector Word64
                       } deriving (Functor, Foldable, Traversable)

parse :: String -> Valley (Vector Word64)
parse input = Valley { w = length grid
                     , h = length (head grid)
                     , nBlizz = readIns '^'
                     , sBlizz = readIns 'v'
                     , wBlizz = readIns '<'
                     , eBlizz = readIns '>'
                     , walls = readIns '#'
                     }
    where grid = transpose $ lines input
          readIns d = V.fromList $ map (foldr (\c acc -> shiftL acc 1 .|. (if c == d then 1 else 0)) 0) grid

shortestPath :: (Int, Int)-> (Int, Int) -> Valley (Vector Word64) -> (Int, Valley (Vector Word64))
shortestPath (startR, startC) (goalR, goalC) valley = runST $ do
  frontier <- MV.replicate (w valley) (0 :: Word64)
  MV.modify frontier (`setBit` startR) startC
  traverse V.thaw valley >>= go 0 frontier
    where go !t frontier val@Valley{..} = do
            done <- (`testBit` goalR) <$> MV.read frontier goalC
            if done then (t,) <$> traverse V.freeze val
            else do
              pwBliz <- V.freeze wBlizz
              peBliz <- V.freeze eBlizz
              pFrontier <- V.freeze frontier
              forM_ [0..w-1] $ \c -> do
                MV.modify nBlizz (\v -> (shiftR v 1 .|. shiftL (v .&. 2) (h - 3)) .&. complement (walls ! c)) c
                MV.modify sBlizz (\v -> (shiftL v 1 .|. shiftR v (h - 3) .&. 2) .&. complement (walls ! c)) c
                MV.write wBlizz c (pwBliz ! (c `mod` (w - 2) + 1))
                MV.write eBlizz c (peBliz ! ((c - 2) `mod` (w - 2) + 1))
                MV.modify frontier (\v -> v .|. shiftR (pFrontier ! c) 1 .|.
                                          shiftL (pFrontier ! c) 1 .|.
                                          (pFrontier ! ((c + 1) `mod` w)) .|.
                                          (pFrontier ! ((c - 1) `mod` w))) c
                MV.modifyM frontier (\v -> do
                                       nb <- MV.read nBlizz c
                                       sb <- MV.read sBlizz c
                                       wb <- MV.read wBlizz c
                                       eb <- MV.read eBlizz c
                                       pure $ v .&. complement ((walls ! c) .|. nb .|. sb .|. wb .|. eb)) c
              go (t+1) frontier val

part1 :: String -> Int
part1 input = fst $ shortestPath (0, 1) (h valley - 1, w valley - 2) valley
    where valley = parse input

part2 :: String -> Int
part2 input = flip evalState val $ do
  t1 <- state $ shortestPath start goal
  t2 <- state $ shortestPath goal start
  t3 <- state $ shortestPath start goal
  pure $ t1 + t2 + t3
    where val = parse input
          start = (0, 1)
          goal = (h val - 1, w val - 2)
