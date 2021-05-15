module Year2017.Day21
    ( part1
    , part2
    ) where

import Data.Array.Unboxed
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import Data.List (transpose)
import Data.List.Split


type ExpansionMap = HashMap String String
type Image = UArray (Int, Int) Char

parseExpansions :: String -> ExpansionMap
parseExpansions = M.fromList . concatMap parse . lines
    where parse x = let [k, rest] = splitOn " => " x
                        v = filter (/= '/') rest
                    in map (,v) $ orientations k
          orientations str = let grid = splitOn "/" str
                             in map concat $ concatMap (\x -> [x, mirror x])
                                    $ take 4 $ iterate rotate grid
          mirror = map reverse
          rotate = mirror . transpose

sqr :: a -> a -> ((a, a), (a, a))
sqr a b = ((a, a), (b, b))

expandImage :: ExpansionMap -> Image -> Image
expandImage m arr = array (sqr 0 (size' - 1)) $ concat $ zipWith expandGrid coords coords'
    where size = (+1) $ fst $ snd $ bounds arr
          size' = size + grids
          (gSpan, grids) = if size `mod` 2 == 0
                           then (2, size `div` 2)
                           else (3, size `div` 3)
          coords = [ ((r, c), (r + gSpan-1, c + gSpan-1))
                   | r <- [0, gSpan .. size - 1]
                   , c <- [0, gSpan .. size - 1] ]
          coords' = zipWith offsetCoords coords $ range (sqr 0 (grids - 1))
          offsetCoords ((rowStart, colStart), (rowEnd, colEnd)) (rowOffset, colOffset) =
              ( (rowStart + rowOffset, colStart + colOffset)
              , (rowEnd + rowOffset + 1, colEnd + colOffset + 1) )
          expandGrid c c' = zip (range c') $ m M.! map (arr !) (range c)

countPxAfterExpanding :: Int -> String -> Int
countPxAfterExpanding n input =
    length $ filter (=='#') $ elems $ (!! n) $ iterate (expandImage m) start
    where m = parseExpansions input
          start = listArray (sqr 0 2) ".#...####"

part1 :: String -> Int
part1 = countPxAfterExpanding 5

part2 :: String -> Int
part2 = countPxAfterExpanding 18
