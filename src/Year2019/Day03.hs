{-# LANGUAGE NoFieldSelectors, OverloadedRecordDot #-}

module Year2019.Day03
    ( part1
    , part2
    ) where

import Data.List (tails)
import Data.List.Split (splitOn)
import Data.Traversable
import Linear.V2

data Orientation = V | H deriving (Eq)

data Segment = Segment { o :: Orientation
                       , a :: V2 Int
                       , b :: V2 Int
                       , d :: Int
                       , r :: Bool
                       }

parseWire :: String -> [Segment]
parseWire = snd . mapAccumL go (V2 0 0, 0) . splitOn ","
    where go :: (V2 Int, Int) -> String -> ((V2 Int, Int), Segment)
          go (pos, steps) (p:ps) = let (o, u) = case p of
                                                  'U' -> (V, V2 0 1)
                                                  'D' -> (V, V2 0 (-1))
                                                  'L' -> (H, V2 (-1) 0)
                                                  'R' -> (H, V2 1 0)
                                                  _ -> error "Unknown direction"
                                       n = read ps
                                       pos' = pos + pure n * u
                                       (d, a, b, r) = if pos < pos'
                                                      then (steps, pos, pos', False)
                                                      else (steps+n, pos', pos, True)
                                       steps' = steps + n
                                   in ((pos', steps'), Segment o a b d r)
          go _ _ = error "impossible"

intersections :: [Segment] -> [Segment] -> [(Int, Int)]
intersections self other = [ (abs vax + abs hay
                             , hs.d + (if hs.r then -1 else 1) * abs (hax - vax) +
                               vs.d + (if vs.r then -1 else 1) * abs (vay - hay) )
                           | w1 <- self, w2 <- other
                           , w1.o /= w2.o
                           , let (hs, vs) = if w1.o == H then (w1, w2) else (w2, w1)
                           , let (V2 hax hay, V2 hbx _) = (hs.a, hs.b)
                           , let (V2 vax vay, V2 _ vby) = (vs.a, vs.b)
                           , hax <= vax && vax <= hbx && vay <= hay && hay <= vby ]

solve :: ((Int, Int) -> Int) -> String -> Int
solve f input = minimum [ f int | (w1:ws) <- init (tails (map parseWire (lines input)))
                        , w2 <- ws
                        , int <- intersections w1 w2 ]

part1 :: String -> Int
part1 = solve fst

part2 :: String -> Int
part2 = solve snd
