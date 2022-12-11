module Year2022.Day10
    ( part1
    , part2
    ) where

import Data.List.Split

import Ocr

run :: String -> [Int]
run = scanl (+) 1 . map go . init . words
    where go "addx" = 0
          go "noop" = 0
          go n = read n

part1 :: String -> Int
part1 = sum . map (uncurry (*)) . filter ((== 20) . (`mod` 40) . fst) . zip [1..] . run

part2 :: String -> String
part2 = parseLetters . unlines . chunksOf 40
        . zipWith (\c x -> if abs (c `mod` 40 - x) <= 1 then '#' else ' ') [0..] . run
