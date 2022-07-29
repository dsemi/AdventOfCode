module Year2018.Day23
    ( part1
    , part2
    ) where

import Control.Monad
import Data.Maybe
import Data.List (foldl', maximumBy)
import Data.Ord
import Linear.V3
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

type Coord = V3 Int

data Nanobot = Nanobot { pos :: Coord
                       , radius :: Int
                       } deriving (Show)

parseNanobots :: String -> [Nanobot]
parseNanobots = map (fromJust . parseMaybe @() nanobot) . lines
    where nanobot = do
            [x, y, z] <- between (string "pos=<") (string ">")
                         $ signed (pure ()) decimal `sepBy` char ','
            r <- string ", r=" *> decimal
            pure $ Nanobot (V3 x y z) r

nanoInRange :: Nanobot -> Coord -> Bool
nanoInRange (Nanobot p r) p' = sum (abs $ p - p') <= r

part1 :: String -> Int
part1 input = let ns = parseNanobots input
                  maxBot = maximumBy (comparing radius) ns
              in length $ filter (nanoInRange maxBot . pos) ns

data Cube = Cube { lo :: (V3 Int), hi :: (V3 Int) }

children :: Cube -> [Cube]
children (Cube (V3 x0 y0 z0) (V3 x1 y1 z1)) =
    [ Cube (V3 (midX+1) (midY+1) (midZ+1)) (V3 x1 y1 z1)
    , Cube (V3 (midX+1) (midY+1) z0) (V3 x1 y1 midZ)
    , Cube (V3 (midX+1) y0 (midZ+1)) (V3 x1 midY z1)
    , Cube (V3 (midX+1) y0 z0) (V3 x1 midY midZ)
    , Cube (V3 x0 (midY+1) (midZ+1)) (V3 midX y1 z1)
    , Cube (V3 x0 (midY+1) z0) (V3 midX y1 midZ)
    , Cube (V3 x0 y0 (midZ+1)) (V3 midX midY z1)
    , Cube (V3 x0 y0 z0) (V3 midX midY midZ)
    ]
    where (midX, midY, midZ) = ((x0 + x1) `div` 2, (y0 + y1) `div` 2, (z0 + z1) `div` 2)

cubeInRange :: Cube -> Nanobot -> Bool
cubeInRange (Cube (V3 x0 y0 z0) (V3 x1 y1 z1)) n = nanoInRange n p
    where V3 x y z = pos n
          p = V3 (max x0 (min x1 x)) (max y0 (min y1 y)) (max z0 (min z1 z))

part2 :: String -> Int
part2 input = sum $ abs $ lo $ go cube
    where ns = parseNanobots input
          cube = foldl' (\c n -> Cube (liftM2 min (lo c) (subtract (radius n) <$> pos n))
                                 (liftM2 max (hi c) ((+radius n) <$> pos n)))
                 (Cube (V3 maxBound maxBound maxBound) (V3 minBound minBound minBound)) ns
          nanosInRange c = length $ filter (cubeInRange c) ns
          go c@(Cube (V3 x0 y0 z0) (V3 x1 y1 z1))
              | x0 >= x1 && y0 >= y1 && z0 >= z1 = c
              | otherwise = go $ maximumBy (comparing nanosInRange) $ children c
