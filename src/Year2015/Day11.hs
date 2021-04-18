module Year2015.Day11
    ( part1
    , part2
    ) where


incrStr :: String -> String
incrStr = reverse . step . reverse
    where step [] = []
          step (x:xs)
              | x == 'z' = 'a' : step xs
              | otherwise = succ x : xs

isValid :: String -> Bool
isValid s = or (zipWith3 increasing s (tail s) (tail (tail s)))
            && length (filter id (scanl1 (\b x -> not b && x) (zipWith (==) s (tail s)))) >= 2
            && not (any (`elem` s) "iol")
    where increasing a b c = succ a == b && succ b == c

part1 :: String -> String
part1 = head . filter isValid . tail . iterate incrStr

part2 :: String -> String
part2 = (!! 1) . filter isValid . tail . iterate incrStr
