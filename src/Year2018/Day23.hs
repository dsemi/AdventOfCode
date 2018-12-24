{-# LANGUAGE TypeApplications #-}

module Year2018.Day23
    ( part1
    , part2
    ) where

import Data.Maybe
import Data.List (maximumBy)
import Data.Ord
import Data.SBV
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer


type Coord = (Integer, Integer, Integer)

data Nanobot = Nanobot { pos :: Coord
                       , range :: Integer
                       } deriving (Show)

parseNanobots :: String -> [Nanobot]
parseNanobots = map (fromJust . parseMaybe @() nanobot) . lines
    where nanobot = do
            [x, y, z] <- between (string "pos=<") (string ">")
                         $ signed (pure ()) decimal `sepBy` char ','
            r <- string ", r=" *> decimal
            pure $ Nanobot (x, y, z) r

inRange' :: Nanobot -> Coord -> Bool
inRange' (Nanobot (x, y, z) r) (x', y', z') =
    abs (x - x') + abs (y - y') + abs (z - z') <= r

part1 :: String -> Int
part1 input = let ns = parseNanobots input
                  maxBot = maximumBy (comparing range) ns
              in length $ filter (inRange' maxBot . pos) ns

dist :: (OrdSymbolic a, Num a) => a -> a -> a -> a -> a -> a -> a
dist x y z x' y' z' = abs' (x - x') + abs' (y - y') + abs' (z - z')
    where abs' n = ite (n .< 0) (negate n) n

inRange'' :: (SymWord a, Num a) => SInteger -> SInteger -> SInteger -> Nanobot -> SBV a
inRange'' x y z (Nanobot (x', y', z') r) =
    oneIf $ (.<= literal r) $ dist (literal x') (literal y') (literal z') x y z

part2 :: String -> IO (Maybe Integer)
part2 input = do
  let ns = parseNanobots input
  LexicographicResult res <- optimize Lexicographic $ do
    x <- sInteger "x"
    y <- sInteger "y"
    z <- sInteger "z"
    maximize "numNanobotsInRange" $ sum $ map (inRange'' x y z :: Nanobot -> SInteger) ns
    minimize "distFromOrigin" $ dist 0 0 0 x y z
  pure $ getModelValue "distFromOrigin" res
