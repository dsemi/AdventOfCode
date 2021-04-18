module Year2020.Day10
    ( part1
    , part2
    ) where

import Control.Arrow
import qualified Data.IntMap as M
import Data.List (sort)


parse :: String -> [Int]
parse input = 0 : (ns ++ [last ns + 3])
    where ns = sort $ map read $ lines input

joltz :: [Int] -> Int
joltz ns = length (filter (==1) js) * length (filter (==3) js)
    where js = zipWith (-) (tail ns) ns

part1 :: String -> Int
part1 = joltz . parse

numValid :: [Int] -> Int
numValid ns = m M.! (last ns)
    where m = M.fromList $ map (id &&& f) ns
          f 0 = 1
          f n = sum $ map (\x -> M.findWithDefault 0 x m) [n-1, n-2, n-3]

part2 :: String -> Int
part2 = numValid . parse
