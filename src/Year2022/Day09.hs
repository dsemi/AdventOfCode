module Year2022.Day09
    ( part1
    , part2
    ) where

import qualified Data.HashSet as S
import Linear.V2

simRope :: Int -> String -> Int
simRope ropeLen = go (S.singleton $ V2 0 0) (replicate ropeLen $ V2 0 0) . lines
    where go :: S.HashSet (V2 Int) -> [V2 Int] -> [String] -> Int
          go tailPos _ [] = S.size tailPos
          go tailPos kss (line : rest) = go tailPos' (last iters) rest
              where [d, n] = words line
                    dp = case head d of
                           'L' -> V2 (-1) 0
                           'R' -> V2 1 0
                           'U' -> V2 0 1
                           'D' -> V2 0 (-1)
                           _ -> error "Malformed input"
                    f a b = let diff = a - b
                            in if maximum (abs diff) > 1 then b + signum diff else b
                    iters = take (read n) $ tail $ iterate (\x -> scanl f (head x + dp) $ tail x) kss
                    tailPos' = foldr S.insert tailPos $ map last iters

part1 :: String -> Int
part1 = simRope 2

part2 :: String -> Int
part2 = simRope 10
