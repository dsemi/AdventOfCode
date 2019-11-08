module Year2016.Day17
    ( part1
    , part2
    ) where

import Utils

import Crypto.Hash.MD5 (hash)
import Data.ByteString.Base16 (encode)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B


type Path = ((Int, Int), ByteString)

isDone :: Path -> Bool
isDone = (==(4, 4)) . fst

neighbors :: Path -> [Path]
neighbors path@((x, y), b)
    | isDone path = []
    | otherwise = filter inBounds . map (apply . snd) $ filter isOpen udlr
    where udlr = zip (B.unpack . B.take 4 . encode $ hash b) "UDLR"
          isOpen = (`elem` "bcdef") . fst
          apply 'U' = ((x, y-1), B.snoc b 'U')
          apply 'D' = ((x, y+1), B.snoc b 'D')
          apply 'L' = ((x-1, y), B.snoc b 'L')
          apply 'R' = ((x+1, y), B.snoc b 'R')
          apply  _  = error "Bad state"
          inBounds ((x', y'), _) = x' > 0 && x' <= 4 && y' > 0 && y' <= 4

part1 :: ByteString -> ByteString
part1 s = B.drop (B.length s) $ snd $ head $ filter isDone $ map snd $ bfs ((1, 1), s) neighbors

part2 :: ByteString -> Int
part2 s = maximum $ map fst $ filter (isDone . snd) $ bfs ((1, 1), s) neighbors
