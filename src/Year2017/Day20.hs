{-# LANGUAGE NamedFieldPuns #-}

module Year2017.Day20
    ( part1
    , part2
    ) where

import Control.Monad (void)
import Data.Hashable (Hashable, hashWithSalt)
import qualified Data.HashSet as S
import Data.Maybe (fromJust)
import Data.Ord (comparing)
import Data.List (foldl', minimumBy)
import Text.Megaparsec
import Text.Megaparsec.Char (char, letterChar, string)
import Text.Megaparsec.Char.Lexer (decimal, signed)

data Vector = Vector Int Int Int deriving (Eq)

data Particle = Particle { position :: Vector
                         , velocity :: Vector
                         , acceleration :: Vector
                         }

manhattan :: Vector -> Int
manhattan (Vector x y z) = abs x + abs y + abs z

plus :: Vector -> Vector -> Vector
plus (Vector x1 y1 z1) (Vector x2 y2 z2) = Vector (x1+x2) (y1+y2) (z1+z2)

instance Ord Vector where
    compare v1 v2 = manhattan v1 `compare` manhattan v2

instance Hashable Vector where
    hashWithSalt s (Vector x y z) = hashWithSalt s (x, y, z)

parseParticles :: String -> [Particle]
parseParticles = map (fromJust . parseMaybe parseParticle) . lines
    where int = signed (pure ()) decimal
          parseVector = do
            void $ letterChar
            [x, y, z] <- between (string "=<") (char '>') $ int `sepBy` char ','
            pure $ Vector x y z
          parseParticle :: Parsec () String Particle
          parseParticle = do
            [p, v, a] <- parseVector `sepBy` string ", "
            pure $ Particle p v a

part1 :: String -> Int
part1 = fst . minimumBy (comparing (acceleration . snd)) . zip [0..] . parseParticles

step :: Particle -> Particle
step (Particle {position, velocity, acceleration}) = Particle position' velocity' acceleration
    where velocity' = velocity `plus` acceleration
          position' = position `plus` velocity'

removeCollisions :: [Particle] -> [Particle]
removeCollisions particles = filter (not . (`S.member` collided) . position) particles
    where collided = snd . foldl' go (S.empty, S.empty) $ map position particles
          go (seen, toRemove) p
              | S.member p seen = (seen, S.insert p toRemove)
              | otherwise = (S.insert p seen, toRemove)

part2 :: String -> Int
part2 = length . (!! 99) . iterate (map step . removeCollisions) . parseParticles
