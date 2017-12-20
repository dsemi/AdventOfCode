module Year2015.Day10
    ( part1
    , part2
    ) where


lookAndSay :: String -> String
lookAndSay [] = error "Invalid input"
lookAndSay (y:ys) = go (y, 1) ys
    where say n c = show n ++ [c]
          go :: (Char, Int) -> String -> String
          go (c, n) [] = say n c
          go (c, n) (x:xs)
              | x == c = go (c, n+1) xs
              | otherwise = say n c ++ go (x, 1) xs

part1 :: String -> Int
part1 =  length . (!! 40) . iterate lookAndSay

part2 :: String -> Int
part2 = length . (!! 50) . iterate lookAndSay
