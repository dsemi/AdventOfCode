module Year2018.Day09
    ( part1
    , part2
    ) where

import Control.Lens
import Data.ByteString (ByteString)
import Data.List.PointedList.Circular
import qualified Data.IntMap.Strict as M

import Scanf

parse :: ByteString -> (Int, Int)
parse = (,) |. scanf [fmt|%d players; last marble is worth %d points|]

play :: (Int, Int) -> Maybe Int
play (n, s) = go 1 M.empty (singleton 0)
    where go p m c
              | p == s+1 = Just $ maximum $ M.elems m
              | p `rem` 23 /= 0 = go (p+1) m $ insertRight p (next c)
              | otherwise = let c' = moveN (-7) c
                                m' = M.insertWith (+) (p `rem` n) (p + c' ^. focus) m
                            in delete c' >>= go (p+1) m'

part1 :: ByteString -> Maybe Int
part1 = play . parse

part2 :: ByteString -> Maybe Int
part2 = play . (_2 *~ 100) . parse
