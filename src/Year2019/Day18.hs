module Year2019.Day18
    ( part1
    , part2
    ) where

import Control.Lens
import Data.Array.Unboxed
import Data.Bits
import Data.Char
import Data.Function.Memoize
import qualified Data.IntSet as S
import Linear.V2

c2Int :: Char -> Int
c2Int c = ord c - ord 'a'

deriveMemoizable ''V2

parseMaze :: String -> UArray (V2 Int) Char
parseMaze input = array (fst (head grid), fst (last grid)) grid
    where grid = [ ((V2 x y), c) | (y, row) <- zip [0..] (lines input), (x, c) <- zip [0..] row ]

distsToKeys :: UArray (V2 Int) Char -> Int -> V2 Int -> [(Char, Int, V2 Int)]
distsToKeys grid found start = go (S.singleton (enc start)) $ neighbors (start, 0)
    where cols = succ $ view _x $ snd $ bounds grid
          enc (V2 x y) = x * cols + y
          neighbors (V2 x y, depth) =
              [ (coord, depth + 1) | coord <- [V2 (x-1) y, V2 (x+1) y, V2 x (y-1), V2 x (y+1)]
              , let v = grid ! coord
              , v /= '#'
              , not (isUpper v) || testBit found (c2Int (toLower v))
              ]
          go _       [] = []
          go visited (node@(pos, depth) : nodes)
              | S.member ePos visited = go visited nodes
              | isLower k && not (testBit found $ c2Int k) = (k, depth, pos) : go visited' nodes
              | otherwise = go visited' (nodes ++ neighbors node)
              where visited' = S.insert ePos visited
                    k = grid ! pos
                    ePos = enc pos

search :: Char -> UArray (V2 Int) Char -> Int
search key grid = memoFix2 go keyPoss 0
    where keyPoss = map fst $ filter ((==key) . snd) $ assocs grid
          d2k = memoize2 $ distsToKeys grid
          go f starts found = if null ans then 0 else minimum ans
              where ans = map (\(i, (ch, dist, pos)) ->
                                   dist + f (starts & ix i .~ pos) (setBit found $ c2Int ch))
                          $ concat $ zipWith (\i -> map (i,) . d2k found) [0..] starts

part1 :: String -> Int
part1 = search '@' . parseMaze

quadrants :: UArray (V2 Int) Char -> UArray (V2 Int) Char
quadrants maze = maze // zip (range (V2 39 39, V2 41 41)) "@#@###@#@"

part2 :: String -> Int
part2 = search '@' . quadrants . parseMaze
