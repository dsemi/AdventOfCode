module Year2016.Day02
    ( part1
    , part2
    ) where

import Control.Lens (_1, _2, over)
import Data.Array ((!), Array, assocs, bounds, listArray)
import Data.Ix (inRange)


dirFunc 'U' = over _1 pred
dirFunc 'R' = over _2 succ
dirFunc 'D' = over _1 succ
dirFunc 'L' = over _2 pred

type Coord = (Int, Int)
type Pad = Array Coord Char

pad :: Pad
pad = listArray ((0, 0), (2, 2)) [ '1', '2', '3'
                                 , '4', '5', '6'
                                 , '7', '8', '9' ]

findCode :: Char -> Pad -> String -> String
findCode start pad = go start . lines
    where nextKey key dir
              | inRange (bounds pad) coord
                && val /= ' ' = val
              | otherwise     = key
              where coord = dirFunc dir $ findCoord key
                    val   = pad ! coord
          findCoord c = fst . head . filter ((==c) . snd) $ assocs pad
          go _ [] = []
          go s (x:xs) = c : go c xs
              where c = foldl nextKey s x

part1 :: String -> String
part1 = findCode '5' pad

pad' :: Pad
pad' = listArray ((0, 0), (4, 4)) [ ' ', ' ', '1', ' ', ' '
                                  , ' ', '2', '3', '4', ' '
                                  , '5', '6', '7', '8', '9'
                                  , ' ', 'A', 'B', 'C', ' '
                                  , ' ', ' ', 'D', ' ', ' ' ]

part2 :: String -> String
part2 = findCode '5' pad'
