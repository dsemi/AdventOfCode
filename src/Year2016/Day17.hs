{-# LANGUAGE TupleSections #-}

module Year2016.Day17
    ( part1
    , part2
    ) where

import Utils (aStar)

import Control.Lens (_1, _2, over)
import Crypto.Hash.MD5 (hash)
import Data.ByteString.Base16 (encode)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Hashable (Hashable)
import qualified Data.IntSet as I
import qualified Data.HashSet as S


type Path = ((Int, Int), ByteString)

isDone :: Path -> Bool
isDone = (==(4, 4)) . fst

start :: ByteString -> Path
start b = ((1, 1), b)

heuristic :: Path -> Int
heuristic ((x, y), b) = abs (4-x) + abs (4-y) + B.length b

neighbors :: Path -> [Path]
neighbors (xy, b) = let udlr = zip (B.unpack . B.take 4 . encode $ hash b) "UDLR"
                    in filter (inBounds . fst) . map (apply xy b . snd) $ filter (isOpen . fst) udlr
    where isOpen          = (`elem` "bcdef")
          apply xy b 'U'  = (over _2 (subtract 1) xy, B.snoc b 'U')
          apply xy b 'D'  = (over _2 (+1) xy, B.snoc b 'D')
          apply xy b 'L'  = (over _1 (subtract 1) xy, B.snoc b 'L')
          apply xy b 'R'  = (over _1 (+1) xy, B.snoc b 'R')
          inBounds (x, y) = x > 0 && x <= 4 && y > 0 && y <= 4

part1 :: ByteString -> ByteString
part1 s = let (Just (_, (_, ans))) = aStar (start s) isDone heuristic neighbors
          in B.drop (B.length s) ans

bfs :: (Eq a, Hashable a) => a -> (a -> Bool) -> (a -> [a]) -> Int
bfs start isFinished neighbors = search I.empty S.empty [(0, start)]
    where search ds visited [] = I.findMax ds
          search ds visited ((d, node):ns)
              | isFinished node       = search (I.insert d ds) visited' ns
              | S.member node visited = search ds visited ns
              | otherwise             = search ds visited' (ns ++ map (d+1,) (neighbors node))
              where visited' = S.insert node visited

part2 :: ByteString -> Int
part2 s = bfs (start s) isDone neighbors
