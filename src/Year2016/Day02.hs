module Year2016.Day02
    ( part1
    , part2
    ) where

import Control.Lens (_1, _2, over)
import Data.Array ((!), Array, assocs, bounds, listArray)
import Data.Ix (inRange)


type Coord = (Int, Int)
type Pad = Array Coord Char

dirFunc :: Char -> Coord -> Coord
dirFunc 'U' = over _1 pred
dirFunc 'R' = over _2 succ
dirFunc 'D' = over _1 succ
dirFunc 'L' = over _2 pred

pad :: [String] -> Pad
pad s = let r = length s
            c = length (head s)
        in listArray ((1, 1), (r, c)) $ concat s

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
part1 = findCode '5' $ pad [ "123"
                           , "456"
                           , "789" ]

part2 :: String -> String
part2 = findCode '5' $ pad [ "  1  "
                           , " 234 "
                           , "56789"
                           , " ABC "
                           , "  D  " ]
