module Year2019.Day06
    ( part1
    , part2
    ) where

import Control.Arrow
import qualified Data.Map.Strict as M
import Data.List (unfoldr)
import Data.Tuple

import DaysTH
import Utils


$(buildProb)

parseOrbits :: String -> [(String, String)]
parseOrbits = map ((id *** tail) . break (==')')) . lines

part1' :: String -> Int
part1' = sum . map fst . bfs' . M.fromListWith (++) . map (id *** (:[])) . parseOrbits
    where bfs' orbitMap = bfs "COM" (\x -> M.findWithDefault [] x orbitMap)

part2' :: String -> Int
part2' input = transfers "YOU" "SAN"
    where invTree = map swap $ parseOrbits input
          pathFromCom obj = reverse $ unfoldr (fmap (id &&& id) . (`lookup` invTree)) obj
          transfers a b = go (pathFromCom a) (pathFromCom b)
          go xs ys = length xs + length ys - 2 * length (takeWhile id (zipWith (==) xs ys))
