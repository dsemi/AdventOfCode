module Year2016.Day01
    ( part1
    , part2
    ) where

import Control.Lens
import Data.String.Utils (split)
import qualified Data.HashSet as S


-- North East South West
directions :: [(Int, Int) -> (Int, Int)]
directions = [ over _2 succ
             , over _1 succ
             , over _2 pred
             , over _1 pred
             ]

path :: String -> [(Int, Int)]
path = go 0 (0, 0) . split ", "
    where go _ _ [] = []
          go dir pos ((d:n):xs) = pathPart ++ go newDir (last pathPart) xs
              where dirShift = if d == 'L' then -1 else 1
                    newDir = (dir + dirShift) `mod` 4
                    dirFun = directions !! newDir
                    pathPart = take (read n) . tail $ iterate dirFun pos

manhattanDist :: (Int, Int) -> Int
manhattanDist (a, b) = abs a + abs b

part1 :: String -> String
part1 = show . manhattanDist . last . path

findDup :: [(Int, Int)] -> (Int, Int)
findDup = go S.empty
    where go s []     = undefined
          go s (x:xs)
              | S.size s == S.size s' = x
              | otherwise = go s' xs
              where s' = S.insert x s

part2 :: String -> String
part2 = show . manhattanDist . findDup . path
