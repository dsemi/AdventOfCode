{-# LANGUAGE QuasiQuotes #-}

module Year2018.Day09
    ( part1
    , part2
    ) where

import Control.Lens hiding (re)
import Data.List.PointedList.Circular
import qualified Data.IntMap.Strict as M
import Text.Regex.PCRE.Heavy


parse :: String -> (Int, Int)
parse = (\[a, b] -> (a, b)) . map read . snd . head . scan regex
    where regex = [re|(\d+) players; last marble is worth (\d+) points|]

play :: (Int, Int) -> Maybe Int
play (n, s) = go 1 M.empty (singleton 0)
    where go p m c
              | p == s+1 = Just $ maximum $ M.elems m
              | p `rem` 23 /= 0 = go (p+1) m $ insertRight p (next c)
              | otherwise = let c' = moveN (-7) c
                                m' = M.insertWith (+) (p `rem` n) (p + c' ^. focus) m
                            in delete c' >>= go (p+1) m'

part1 :: String -> Maybe Int
part1 = play . parse

part2 :: String -> Maybe Int
part2 = play . (_2 *~ 100) . parse
