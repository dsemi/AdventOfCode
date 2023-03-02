{-# LANGUAGE BangPatterns, RecordWildCards, NamedFieldPuns #-}

module Year2022.Day24
    ( part1
    , part2
    ) where

import Control.Monad
import Control.Monad.ST
import Control.Monad.State.Strict
import Data.Bits
import Data.List (transpose)
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Word

data Valley = Valley { w :: Int
                     , h :: Int
                     , nBlizz :: Vector Word64
                     , sBlizz :: Vector Word64
                     , wBlizz :: Vector Word64
                     , eBlizz :: Vector Word64
                     , walls :: Vector Word64
                     }

parse :: String -> Valley
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

shortestPath :: (Int, Int)-> (Int, Int) -> Valley -> (Int, Valley)
shortestPath (startR, startC) (goalR, goalC) (Valley{..}) = runST $ do
  frontier <- MV.replicate w (0 :: Word64)
  MV.modify frontier (`setBit` startR) startC
  (nb, sb, wb, eb) <- (,,,) <$> V.thaw nBlizz <*> V.thaw sBlizz <*> V.thaw wBlizz <*> V.thaw eBlizz
  t <- go 0 frontier nb sb wb eb
  (nb', sb', wb', eb') <- (,,,) <$> V.freeze nb <*> V.freeze sb <*> V.freeze wb <*> V.freeze eb
  pure (t, Valley {w, h, nBlizz = nb', sBlizz = sb', wBlizz = wb', eBlizz = eb', walls})
    where go !t frontier nBliz sBliz wBliz eBliz = do
            done <- (`testBit` goalR) <$> MV.read frontier goalC
            if done then pure t
            else do
              pwBliz <- V.freeze wBliz
              peBliz <- V.freeze eBliz
              pFrontier <- V.freeze frontier
              forM_ [0..w-1] $ \c -> do
                MV.modify nBliz (\v -> (shiftR v 1 .|. shiftL (v .&. 2) (h - 3)) .&. complement (walls V.! c)) c
                MV.modify sBliz (\v -> (shiftL v 1 .|. shiftR v (h - 3) .&. 2) .&. complement (walls V.! c)) c
                MV.write wBliz c (pwBliz V.! ((c - 2 + w) `mod` (w - 2) + 1))
                MV.write eBliz c (peBliz V.! ((c - 4 + w) `mod` (w - 2) + 1))
                MV.modify frontier (\v -> v .|. shiftR (pFrontier V.! c) 1 .|.
                                          shiftL (pFrontier V.! c) 1 .|.
                                          (pFrontier V.! ((c + 1) `mod` w)) .|.
                                          (pFrontier V.! ((c - 1) `mod` w))) c
                MV.modifyM frontier (\v -> do
                                       (nb, sb, wb, eb) <- (,,,) <$> MV.read nBliz c <*>
                                                           MV.read sBliz c <*>
                                                           MV.read wBliz c <*>
                                                           MV.read eBliz c
                                       pure $ v .&. complement ((walls V.! c) .|. nb .|. sb .|. wb .|. eb)) c
              go (t+1) frontier nBliz sBliz wBliz eBliz

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
