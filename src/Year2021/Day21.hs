{-# LANGUAGE BangPatterns, FlexibleContexts, OverloadedStrings #-}

module Year2021.Day21
    ( part1
    , part2
    ) where

import Control.Monad.State.Strict
import Data.Maybe
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)

parsePlayers :: String -> (Int, Int)
parsePlayers = fromJust . parseMaybe @() plrs
    where plr = fmap pred $ "Player " *> digitChar *> " starting position: " *> decimal
          plrs = (,) <$> plr <* char '\n' <*> plr

part1 :: String -> Int
part1 input = let (p1, p2) = parsePlayers input
              in go p1 p2 0 0 0 $ cycle [1..100]
    where go !p1 !p2 !p1s !p2s !n (a:b:c:gs)
              | p2s < 1000 = let p1' = (p1 + a + b + c) `mod` 10
                             in go p2 p1' p2s (p1s + p1' + 1) (n+3) gs
              | otherwise = min p1s p2s * n

probs :: [(Int, Int)]
probs = [(3, 1), (4, 3), (5, 6), (6, 7), (7, 6), (8, 3), (9, 1)]

type Cache = HashMap (Int, Int, Int, Int) (Int, Int)

solve :: (MonadState Cache m) => Int -> Int -> Int -> Int -> m (Int, Int)
solve !p1 !p2 !s1 !s2
    | s1 >= 21 = pure (1, 0)
    | s2 >= 21 = pure (0, 1)
    | otherwise = do
  m <- get
  case M.lookup (p1, p2, s1, s2) m of
    Just v -> pure v
    Nothing -> do
      let go (x, y) (d, n) = do
            let newP1 = (p1 + d) `mod` 10
            (x1, y1) <- solve p2 newP1 s2 (s1 + newP1 + 1)
            pure (x + n * y1, y + n * x1)
      ans <- foldM go (0, 0) probs
      modify' $ M.insert (p1, p2, s1, s2) ans
      pure ans

part2 :: String -> Int
part2 input = let (p1, p2) = parsePlayers input
                  (x, y) = evalState (solve p1 p2 0 0) M.empty
              in max x y
