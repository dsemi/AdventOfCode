module Year2020.Day03
    ( part1
    , part2
    ) where

countTrees :: Int -> Int -> String -> Int
countTrees right down = sum . map (\xs -> if (head (head xs)) == '#' then 1 else 0)
                        . takeWhile (not . null) . iterate (map (drop right) . drop down)
                        . map cycle . lines

part1 :: String -> Int
part1 = countTrees 3 1

part2 :: String -> Int
part2 s = product [ countTrees 1 1 s
                  , countTrees 3 1 s
                  , countTrees 5 1 s
                  , countTrees 7 1 s
                  , countTrees 1 2 s ]
