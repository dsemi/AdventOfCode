module Year2019.Day19
    ( part1
    , part2
    ) where

import Year2019.IntCode


isPulled :: Memory -> Int -> Int -> Bool
isPulled mem x y = (== 1) $ head $ runWithInput [x, y] mem

part1 :: String -> Int
part1 (parse -> mem) = length $ filter id $ isPulled mem <$> [0..49] <*> [0..49]

findSquare :: Memory -> Int -> Int -> (Int, Int)
findSquare mem xSize ySize = go 0 0
    where go x y
              | isPulled mem (x + xSize - 1) y = (x, y)
              | isPulled mem x (y + ySize) = go x (y+1)
              | otherwise = go (x+1) (y+1)

part2 :: String -> Int
part2 (parse -> mem) = x * 10000 + y
    where (x, y) = findSquare mem 100 100
