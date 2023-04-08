{-# LANGUAGE BangPatterns #-}

module Year2017.Day22
    ( part1
    , part2
    ) where

import Control.Monad
import Control.Monad.ST
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Word

data NodeState = Cleaned | Weakened | Infected | Flagged deriving (Enum)

turn :: Word8 -> NodeState -> Word8
turn d  Cleaned = (d + 3) `rem` 4
turn d Weakened = d
turn d Infected = (d + 1) `rem` 4
turn d  Flagged = (d + 2) `rem` 4

countInfections :: Int -> (NodeState -> NodeState) -> String -> Int
countInfections bursts nextState input =
    runST $ do
      grid <- MV.replicate (gridSize^2) $ fromEnum Cleaned
      forM_ (zip [mid-12..mid+12] $ lines input) $ \(r, row) ->
          forM_ (zip [mid-12..mid+12] row) $ \(c, v) ->
              when (v == '#') $ MV.write grid (r * gridSize + c) $ fromEnum Infected
      go grid bursts 0 (mid*gridSize + mid) 3
    where gridSize = 1000
          mid = gridSize `div` 2
          next = fromEnum . nextState . toEnum
          go _ 0 !count _ _ = pure count
          go grid n !count pos dir = do
            dir' <- turn dir . toEnum <$> MV.read grid pos
            MV.modify grid next pos
            count' <- (\case Infected -> count + 1
                             _ -> count) . toEnum <$> MV.read grid pos
            let pos' = case dir' of
                         0 -> pos + 1
                         1 -> pos + gridSize
                         2 -> pos - 1
                         3 -> pos - gridSize
                         _ -> undefined
            go grid (n-1) count' pos' dir'

part1 :: String -> Int
part1 = countInfections 10000 f
    where f Cleaned  = Infected
          f Infected = Cleaned
          f _        = error "Invalid state"

part2 :: String -> Int
part2 = countInfections 10000000 f
    where f Cleaned  = Weakened
          f Weakened = Infected
          f Infected = Flagged
          f Flagged  = Cleaned
