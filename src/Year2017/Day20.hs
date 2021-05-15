{-# LANGUAGE NamedFieldPuns #-}

module Year2017.Day20
    ( part1
    , part2
    ) where

import Control.Monad (void)
import qualified Data.HashSet as S
import Data.Maybe (fromJust)
import Data.Ord (comparing)
import Data.List (foldl', minimumBy)
import Linear.V3
import Text.Megaparsec
import Text.Megaparsec.Char (char, letterChar, string)
import Text.Megaparsec.Char.Lexer (decimal, signed)

data Particle = Particle { position :: V3 Int
                         , velocity :: V3 Int
                         , acceleration :: V3 Int
                         }

parseParticles :: String -> [Particle]
parseParticles = map (fromJust . parseMaybe parseParticle) . lines
    where int = signed (pure ()) decimal
          parseVector = do
            void $ letterChar
            [x, y, z] <- between (string "=<") (char '>') $ int `sepBy` char ','
            pure $ V3 x y z
          parseParticle :: Parsec () String Particle
          parseParticle = do
            [p, v, a] <- parseVector `sepBy` string ", "
            pure $ Particle p v a

part1 :: String -> Int
part1 = fst . minimumBy (comparing (sum . abs . acceleration . snd)) . zip [0..] . parseParticles

step :: Particle -> Particle
step (Particle {position, velocity, acceleration}) = Particle position' velocity' acceleration
    where velocity' = velocity + acceleration
          position' = position + velocity'

removeCollisions :: [Particle] -> [Particle]
removeCollisions particles = filter (not . (`S.member` collided) . position) particles
    where collided = snd . foldl' go (S.empty, S.empty) $ map position particles
          go (seen, toRemove) p
              | S.member p seen = (seen, S.insert p toRemove)
              | otherwise = (S.insert p seen, toRemove)

part2 :: String -> Int
part2 = length . (!! 99) . iterate (removeCollisions . map step) . parseParticles
