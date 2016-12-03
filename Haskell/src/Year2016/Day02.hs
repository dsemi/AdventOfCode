module Year2016.Day02
    ( part1
    , part2
    ) where


-- 1 2 3
-- 4 5 6
-- 7 8 9
f d 'U'
    | d `elem` "123" = d
    | otherwise      = pred $ pred $ pred d
f d 'R'
    | d `elem` "369" = d
    | otherwise      = succ d
f d 'D'
    | d `elem` "789" = d
    | otherwise      = succ $ succ $ succ d
f d 'L'
    | d `elem` "147" = d
    | otherwise      = pred d

findCode :: Char -> (Char -> Char -> Char) -> String -> String
findCode s f = go s . lines
    where go _ [] = []
          go s (x:xs) = c : go c xs
              where c = foldl f s x

part1 :: String -> String
part1 = findCode '5' f

--     1
--   2 3 4
-- 5 6 7 8 9
--   A B C
--     D
f' '1' 'D' = '3'
f' '2' 'D' = '6'
f' '2' 'R' = '3'
f' '3' 'U' = '1'
f' '3' 'R' = '4'
f' '3' 'D' = '7'
f' '3' 'L' = '2'
f' '4' 'D' = '8'
f' '4' 'L' = '3'
f' '5' 'R' = '6'
f' '6' 'U' = '2'
f' '6' 'R' = '7'
f' '6' 'D' = 'A'
f' '6' 'L' = '5'
f' '7' 'U' = '3'
f' '7' 'R' = '8'
f' '7' 'D' = 'B'
f' '7' 'L' = '6'
f' '8' 'U' = '4'
f' '8' 'R' = '9'
f' '8' 'D' = 'C'
f' '8' 'L' = '7'
f' '9' 'L' = '8'
f' 'A' 'U' = '6'
f' 'A' 'R' = 'B'
f' 'B' 'U' = '7'
f' 'B' 'R' = 'C'
f' 'B' 'D' = 'D'
f' 'B' 'L' = 'A'
f' 'C' 'U' = '8'
f' 'C' 'L' = 'B'
f' 'D' 'U' = 'B'
f'  s   _  =  s

part2 :: String -> String
part2 = findCode '5' f'
