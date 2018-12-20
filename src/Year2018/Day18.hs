module Year2018.Day18
    ( part1
    , part2
    ) where

import Control.Arrow
import qualified Data.HashMap.Strict as M
import Data.Array.Unboxed


type Coord = (Int, Int)

parseLandscape :: String -> UArray Coord Char
parseLandscape input =
    let ls = lines input
        rows = length ls
        cols = length $ head ls
    in listArray ((0, 0), (cols - 1, rows - 1)) $ concat ls

neighbors :: UArray Coord Char -> Coord -> [Char]
neighbors grid (x, y) = [ grid ! xy | xy <- [ (x-1, y), (x+1, y), (x, y-1), (x, y+1)
                                            , (x-1, y-1), (x-1, y+1), (x+1, y-1), (x+1, y+1)]
                        , inRange bds xy ]
    where bds = bounds grid

step :: UArray Coord Char -> UArray Coord Char
step grid = array (bounds grid) $ map (id &&& nextGen) (range (bounds grid))
    where nextGen xy
              | grid ! xy == '.' && count '|' >= 3 = '|'
              | grid ! xy == '|' && count '#' >= 3 = '#'
              | grid ! xy == '#' && (count '#' < 1 || count '|' < 1) = '.'
              | otherwise = grid ! xy
              where count :: Char -> Int
                    count c = M.lookupDefault 0 c counts
                    counts = M.fromListWith (+) $ map (,1) $ neighbors grid xy

resourceValue :: UArray Coord Char -> Int
resourceValue grid = let es = elems grid
                         ws = length $ filter (=='|') es
                         ls = length $ filter (=='#') es
                     in ws * ls

part1 :: String -> Int
part1 = resourceValue . (!! 10) . iterate step . parseLandscape

resourceValueAt :: Int -> [UArray Coord Char] -> Int
resourceValueAt n = go n M.empty
    where go c m (g:gs)
              | M.member r m && snd (m M.! r) == g =
                  let stepsAway = c `mod` (fst (m M.! r) - c)
                  in resourceValue $ iterate step g !! stepsAway
              | otherwise = go (c-1) (M.insert r (c, g) m) gs
              where r = resourceValue g

part2 :: String -> Int
part2 = resourceValueAt 1000000000 . iterate step . parseLandscape
