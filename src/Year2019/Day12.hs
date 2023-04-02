module Year2019.Day12
    ( part1
    , part2
    ) where

import Control.Lens
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.List (findIndex)
import FlatParse.Basic
import Linear.V1
import Linear.V3

import Utils

parseMoons :: ByteString -> [(V3 Int, V3 Int)]
parseMoons = map parse . B.lines
    where parser = do
            x <- $(string "<x=") *> signedInt
            y <- $(string ", y=") *> signedInt
            z <- $(string ", z=") *> signedInt <* $(string ">")
            pure $ (V3 x y z, V3 0 0 0)
          parse line = case runParser parser line of
                         OK res _ -> res
                         _ -> error "unreachable"

applyGravity :: (Monad m, Num (m Int)) => (m Int, m Int) -> (m Int, m Int) -> (m Int, m Int)
applyGravity (p', _) (p, v) = (p,  v + fmap (pred . fromEnum) (liftM2 compare p' p))

step :: (Monad m, Num (m Int)) => [(m Int, m Int)] -> [(m Int, m Int)]
step moons = map (\(p, v) -> (p + v, v)) $ map (\x -> foldr applyGravity x moons) moons

part1 :: ByteString -> Int
part1 = sum . map (uncurry (*) . over both (sum . abs)) . (!! 1000) . iterate step . parseMoons

findCycle :: (Monad m, Eq (m Int), Num (m Int)) => [(m Int, m Int)] -> Maybe Int
findCycle moons = fmap (+1) $ findIndex (== moons) $ tail $ iterate step moons

part2 :: ByteString -> Maybe Int
part2 (parseMoons -> moons) =
    foldr1 (liftM2 lcm) $ map (\x -> findCycle $ map (over both (V1 . (^. x))) moons) [_x, _y, _z]
