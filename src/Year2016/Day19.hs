module Year2016.Day19
    ( part1
    , part2
    ) where

import Data.Sequence (Seq, ViewL(..))
import qualified Data.Sequence as S

start :: Seq a -> a
start = (\(l :< _) -> l) . S.viewl

takePresent :: (Int -> Int -> Int) -> Int -> Seq Int -> Int
takePresent f index s
    | len > 1 = takePresent f nextIndex $ S.deleteAt indexToDelete s
    | otherwise = start s
    where len           = length s
          indexToDelete = f index len
          updatedIndex  = if indexToDelete < index
                          then index - 1
                          else index
          nextIndex     = (updatedIndex + 1) `mod` (len - 1)

sim :: (Int -> Int -> Int) -> Int -> Int
sim f n = takePresent f 0 $ S.fromList [1..n]

part1 :: String -> Int
part1 = sim (\index len -> (index + 1) `mod` len) . read

part2 :: String -> Int
part2 = sim (\index len -> (index + len `div` 2) `mod` len) . read 
