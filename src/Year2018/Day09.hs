module Year2018.Day09
    ( part1
    , part2
    ) where

import Control.Lens
import Data.ByteString (ByteString)
import Data.List.PointedList.Circular
import qualified Data.IntMap.Strict as M
import FlatParse.Basic

parse :: ByteString -> Maybe (Int, Int)
parse input = case runParser parser input of
                OK res _ -> Just res
                _ -> Nothing
    where parser = do
            a <- anyAsciiDecimalInt <* $(string " players; last marble is worth ")
            b <- anyAsciiDecimalInt <* $(string " points")
            pure (a, b)

play :: (Int, Int) -> Maybe Int
play (n, s) = go 1 M.empty (singleton 0)
    where go p m c
              | p == s+1 = Just $ maximum $ M.elems m
              | p `rem` 23 /= 0 = go (p+1) m $ insertRight p (next c)
              | otherwise = let c' = moveN (-7) c
                                m' = M.insertWith (+) (p `rem` n) (p + c' ^. focus) m
                            in delete c' >>= go (p+1) m'

part1 :: ByteString -> Maybe Int
part1 x = parse x >>= play

part2 :: ByteString -> Maybe Int
part2 x = parse x >>= play . (_2 *~ 100)
