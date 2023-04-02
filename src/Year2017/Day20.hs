{-# LANGUAGE NamedFieldPuns #-}

module Year2017.Day20
    ( part1
    , part2
    ) where

import Control.Monad (void)
import qualified Data.HashSet as S
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Ord (comparing)
import Data.List (foldl', minimumBy)
import FlatParse.Basic
import Linear.V3

import Utils

data Particle = Particle { position :: V3 Int
                         , velocity :: V3 Int
                         , acceleration :: V3 Int
                         }

parseParticles :: ByteString -> [Particle]
parseParticles = map parse . B.lines
    where parseVector = do
            void $ anyAsciiChar >> $(string "=<")
            V3 <$> signedInt <* $(char ',') <*>
               signedInt <* $(char ',') <*>
               signedInt <* $(char '>')
          parseParticle = Particle <$> parseVector <* $(string ", ") <*>
                          parseVector <* $(string ", ") <*>
                          parseVector
          parse line = case runParser parseParticle line of
                         OK res _ -> res
                         _ -> error "unreachable"

part1 :: ByteString -> Int
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

part2 :: ByteString -> Int
part2 = length . (!! 99) . iterate (removeCollisions . map step) . parseParticles
