module Year2018.Day07
    ( part1
    , part2
    ) where

import Control.Arrow ((***))
import Control.Lens
import Data.ByteString (ByteString)
import qualified  Data.ByteString.Char8 as B
import Data.Char (ord)
import qualified Data.HashMap.Strict as M
import Data.HashPSQ (HashPSQ)
import qualified Data.HashPSQ as Q
import Data.HashSet (HashSet)
import qualified Data.HashSet as S
import Data.List (partition, sortBy)
import Data.Maybe (listToMaybe)
import Data.Ord (comparing)

import Scanf

type Queue = HashPSQ Char (Int, Char) (HashSet Char)

parseSteps :: ByteString -> Queue
parseSteps  = foldr (\(k, (p, v)) -> Q.insert k (p, k) v) Q.empty . M.toList
              . M.fromListWith (\(a, b) -> ((a+) *** S.union b))
              . concatMap f . map parse . B.lines
    where f (a, b) = [(a, (0, S.singleton b)), (b, (1, S.empty))]
          parse = apply (,) . scanf [fmt|Step %c must be finished before step %c can begin.|]

solve :: Int -> Queue -> (Int, [Char])
solve n = go 0 []
    where resolveDep :: Char -> Queue -> Queue
          resolveDep k = snd . Q.alter (((),) . (_Just . _1 . _1 -~ 1)) k
          go :: Int -> [(Char, Int, HashSet Char)] -> Queue -> (Int, [Char])
          go c [] queue | Q.null queue = (c, [])
          go c ws queue =
              let steps = maybe 0 (view _2) $ listToMaybe ws
                  (completed, rest) = partition ((==0) . view _2) $ map (_2 -~ steps) ws
                  queue' = foldr (flip $ foldr resolveDep) queue $ map (view _3) completed
                  newWs = map (over _2 (subtract 4 . ord . snd)) $ take (n - length rest)
                          $ sortBy (comparing (view _2)) . fst $ Q.atMostView (0,'Z') queue'
              in over _2 (map (view _1) completed ++)
                 $ go (c+steps) (sortBy (comparing (view _2)) $ rest ++ newWs)
                 $ foldr Q.delete queue' $ map (view _1) newWs

part1 :: ByteString -> String
part1 = snd . solve 1 . parseSteps

part2 :: ByteString -> Int
part2 = fst . solve 5 . parseSteps
