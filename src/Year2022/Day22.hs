module Year2022.Day22
    ( part1
    , part2
    ) where

import Data.Array.Unboxed
import Data.List (foldl')
import Data.List.Split (splitOn)
import Data.Maybe
import Linear.V2
import Text.Megaparsec
import Text.Megaparsec.Char.Lexer

data Dir = R | D | L | U deriving (Enum, Show)

move :: Dir -> V2 Int -> V2 Int
move R (V2 r c) = V2 r (c + 1)
move D (V2 r c) = V2 (r + 1) c
move L (V2 r c) = V2 r (c - 1)
move U (V2 r c) = V2 (r - 1) c

walk :: (V2 Int -> Dir -> Maybe (V2 Int, Dir)) -> String -> Int
walk f input = final $ foldl' go (V2 0 (head (filter ((== '.') . (grid !) . (V2 0)) [0..])), R) instrs
    where pts = splitOn "\n\n" input
          rows = lines $ head pts
          grid :: UArray (V2 Int) Char
          grid = array (V2 0 0, V2 (length rows - 1) (length (head rows) - 1))
                 [ (V2 r c, v) | (r, row) <- zip [0..] rows
                 , (c, v) <- zip [0..] row ]
          instrs = fromJust $ parseMaybe @() parseInstr $ pts !! 1
          parseInstr = some $ eitherP decimal (oneOf "LR")
          go (pos, dir) (Right 'L') = (pos, toEnum $ (fromEnum dir - 1) `mod` 4)
          go (pos, dir) (Right 'R') = (pos, toEnum $ (fromEnum dir + 1) `mod` 4)
          go (pos, dir) (Left n)
              | n == 0 = (pos, dir)
              | grid ! pos' == '#' = (pos, dir)
              | otherwise = go (pos', dir') $ Left $ n-1
              where (pos', dir') = case f pos dir of
                                     Just (p, d) -> (p, d)
                                     Nothing -> (move dir pos, dir)
          go _ _ = error "Bad input"
          final (V2 x y, dir) = 1000*(x + 1) + 4*(y + 1) + fromEnum dir

part1 :: String -> Int
part1 = walk portal
    where portal p d = case (p, d) of
                         (V2 0 c, U) | inRange (50, 99) c -> Just (V2 149 c, U)
                         (V2 149 c, D) | inRange (50, 99) c -> Just (V2 0 c, D)
                         (V2 0 c, U) | inRange (100, 149) c -> Just (V2 49 c, U)
                         (V2 49 c, D) | inRange (100, 149) c -> Just (V2 0 c, D)
                         (V2 r 50, L) | inRange (0, 49) r -> Just (V2 r 149, L)
                         (V2 r 149, R) | inRange (0, 49) r -> Just (V2 r 50, R)
                         (V2 r 50, L) | inRange (50, 99) r -> Just (V2 r 99, L)
                         (V2 r 99, R) | inRange (50, 99) r -> Just (V2 r 50, R)
                         (V2 100 c, U) | inRange (0, 49) c -> Just (V2 199 c, U)
                         (V2 199 c, D) | inRange (0, 49) c -> Just (V2 100 c, D)
                         (V2 r 0, L) | inRange (100, 149) r -> Just (V2 r 99, L)
                         (V2 r 99, R) | inRange (100, 149) r -> Just (V2 r 0, R)
                         (V2 r 0, L) | inRange (150, 199) r -> Just (V2 r 49, L)
                         (V2 r 49, R) | inRange (150, 199) r -> Just (V2 r 0, R)
                         _ -> Nothing

part2 :: String -> Int
part2 = walk portal
    where portal p d = case (p, d) of
                         (V2 0 c, U) | inRange (50, 99) c -> Just (V2 (c+100) 0, R)
                         (V2 r 0, L) | inRange (150, 199) r -> Just (V2 0 (r-100), D)
                         (V2 0 c, U) | inRange (100, 149) c -> Just (V2 199 (c-100), U)
                         (V2 199 c, D) | inRange (0, 49) c -> Just (V2 0 (c+100), D)
                         (V2 r 50, L) | inRange (0, 49) r -> Just (V2 (149-r) 0, R)
                         (V2 r 0, L) | inRange (100, 149) r -> Just (V2 (149-r) 50, R)
                         (V2 r 149, R) | inRange (0, 49) r -> Just (V2 (149-r) 99, L)
                         (V2 r 99, R) | inRange (100, 149) r -> Just (V2 (149-r) 149, L)
                         (V2 49 c, D) | inRange (100, 149) c -> Just (V2 (c-50) 99, L)
                         (V2 r 99, R) | inRange (50, 99) r -> Just (V2 49 (r+50), U)
                         (V2 r 50, L) | inRange (50, 99) r -> Just (V2 100 (r-50), D)
                         (V2 100 c, U) | inRange (0, 49) c -> Just (V2 (c+50) 50, R)
                         (V2 149 c, D) | inRange (50, 99) c -> Just (V2 (c+100) 49, L)
                         (V2 r 49, R) | inRange (150, 199) r -> Just (V2 149 (r-100), U)
                         _ -> Nothing
