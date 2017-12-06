module Year2017.Day06
    ( part1
    , part2
    ) where

import Control.Monad
import qualified Data.HashMap.Strict as M
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

data Cycle = Cycle { lengthToCycle :: Int
                   , lengthOfCycle :: Int
                   }

parseBlocks :: String -> Vector Int
parseBlocks = V.fromList . map read . words

redistributeUntilCycle :: String -> Cycle
redistributeUntilCycle = go 0 M.empty . parseBlocks
    where go c m v
              | M.member serialized m = Cycle c (c - m M.! serialized)
              | otherwise = go (c+1) (M.insert serialized c m) $ redistribute v
              where serialized = show v
          redistribute v = V.modify redist v
              where i = V.maxIndex v
                    val = v V.! i
                    redist v = do
                      MV.write v i 0
                      forM_ (map (`mod` MV.length v) [i+1 .. i+val]) $ MV.modify v (+1)

part1 :: String -> Int
part1 = lengthToCycle . redistributeUntilCycle

part2 :: String -> Int
part2 = lengthOfCycle . redistributeUntilCycle
