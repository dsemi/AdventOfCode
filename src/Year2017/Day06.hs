module Year2017.Day06
    ( part1
    , part2
    ) where

import Control.Monad
import Data.HashMap.Strict (empty, insert, member, (!))
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as M


data Cycle = Cycle { lengthToCycle :: Int
                   , lengthOfCycle :: Int
                   }

redistributeUntilCycle :: String -> Cycle
redistributeUntilCycle = go 0 empty . V.fromList . map read . words
    where go c m v
              | member serialized m = Cycle c $ c - m ! serialized
              | otherwise = go (c+1) (insert serialized c m) $ V.modify redistribute v
              where serialized = show v
          redistribute v = do
            i <- V.maxIndex <$> V.unsafeFreeze v
            val <- M.read v i
            M.write v i 0
            forM_ (map (`mod` M.length v) [i+1 .. i+val]) $
                M.modify v (+1)

part1 :: String -> Int
part1 = lengthToCycle . redistributeUntilCycle

part2 :: String -> Int
part2 = lengthOfCycle . redistributeUntilCycle
