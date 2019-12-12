{-# LANGUAGE ViewPatterns #-}

module Year2019.Day12
    ( part1
    , part2
    ) where

import Control.Lens
import Control.Monad
import Data.List (findIndex)
import Data.Maybe
import Linear.V1
import Linear.V3
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer


parseMoons :: String -> [(V3 Int, V3 Int)]
parseMoons = map (fromJust . parseMaybe parser) . lines
    where parser :: Parsec () String (V3 Int, V3 Int)
          parser = do
            x <- string "<x=" *> signed (pure ()) decimal
            y <- string ", y=" *> signed (pure ()) decimal
            z <- string ", z=" *> signed (pure ()) decimal <* string ">"
            pure $ (V3 x y z, V3 0 0 0)

applyGravity :: (Monad m, Num (m Int)) => (m Int, m Int) -> (m Int, m Int) -> (m Int, m Int)
applyGravity (p', _) (p, v) = (p,  v + fmap (pred . fromEnum) (liftM2 compare p' p))

step :: (Monad m, Num (m Int)) => [(m Int, m Int)] -> [(m Int, m Int)]
step moons = map (\(p, v) -> (p + v, v)) $ map (\x -> foldr applyGravity x moons) moons

part1 :: String -> Int
part1 = sum . map (uncurry (*) . over both (sum . abs)) . (!! 1000) . iterate step . parseMoons

findCycle :: (Monad m, Eq (m Int), Num (m Int)) => [(m Int, m Int)] -> Int
findCycle moons = (+1) $ fromJust $ findIndex (== moons) $ tail $ iterate step moons

part2 :: String -> Int
part2 (parseMoons -> moons) =
    foldr1 lcm $ map (\x -> findCycle $ map (over both (V1 . (^. x))) moons) [_x, _y, _z]
