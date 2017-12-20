module Year2016.Day16
    ( part1
    , part2
    ) where

import Data.Bits
import Data.Char
import qualified Data.Vector.Unboxed as V


dragonChecksum :: Int -> String -> String
dragonChecksum desiredLen ns = let sz = desiredLen `div` until odd (`div` 2) desiredLen
                               in checksum sz
                                      $ V.take desiredLen
                                      $ until ((>=desiredLen) . V.length) curve
                                      $ V.fromList
                                      $ map digitToInt ns
    where curve a = V.concat [a, V.singleton 0, V.map (xor 1) $ V.reverse a]
          checksum n src
              | V.null src = []
              | odd $ V.foldl1' (+) xs = '0' : checksum n rest
              | otherwise              = '1' : checksum n rest
              where (xs, rest) = V.splitAt n src

part1 :: String -> String
part1 = dragonChecksum 272

part2 :: String -> String
part2 = dragonChecksum 35651584
