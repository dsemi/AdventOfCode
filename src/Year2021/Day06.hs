module Year2021.Day06
    ( part1
    , part2
    ) where

solve :: Int -> String -> Int
solve n s = sum $ take 9 $ drop n ms
    where ns = take 9 $ drop 7 $ cycle $ map (\x -> length $ filter (==x) s) ['0'..'8']
          ms = ns ++ zipWith (+) ms (drop 2 ms)

part1 :: String -> Int
part1 = solve 80

part2 :: String -> Int
part2 = solve 256
