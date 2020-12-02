module Year2020.Day02
    ( part1
    , part2
    ) where

replace :: Char -> Char -> String -> String
replace a b = map (\x -> if x == a then b else x)

countValid :: (Int -> Int -> Char -> String -> Bool) -> String -> Int
countValid f input = length $ filter valid $ lines $ replace '-' ' ' input
    where valid line = let [lo, hi, char, pass] = words line
                       in f (read lo) (read hi) (head char) pass

isValid :: Int -> Int -> Char -> String -> Bool
isValid lo hi c s = lo <= numC && numC <= hi
    where numC = length $ filter (==c) s

part1 :: String -> Int
part1 = countValid isValid

isValid' :: Int -> Int -> Char -> String -> Bool
isValid' lo hi c s = (s !! (lo - 1) == c) /= (s !! (hi - 1) == c)

part2 :: String -> Int
part2 = countValid isValid'
