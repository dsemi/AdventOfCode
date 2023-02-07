module Year2022.Day19
    ( part1
    , part2
    ) where

import Data.Bits
import Data.Int
import Data.List (foldl', foldl1')
import Data.Vector.Unboxed (Vector, (!))
import qualified Data.Vector.Unboxed as V
import Data.Word

import Utils

data Blueprint = Blueprint Int32 [Vector Int32] (Vector Int32)

blueprints :: String -> [Blueprint]
blueprints = map bp . lines
    where bp line =
              case findAllInts line of
                [num, oreBotOre, clayBotOre, obsBotOre, obsBotClay, geodeBotOre, geodeBotObs] ->
                    let costs = map V.fromList [ [0, geodeBotObs, 0, geodeBotOre]
                                               , [0, 0, obsBotClay, obsBotOre]
                                               , [0, 0, 0, clayBotOre]
                                               , [0, 0, 0, oreBotOre] ]
                    in Blueprint num costs $ foldl1' (V.zipWith max) costs
                _ -> error "Malformed input"

sim :: Int32 -> Blueprint -> Int32
sim time (Blueprint _ costs maxCosts) = dfs 0 time (V.fromList [0, 0, 0, 0]) (V.fromList [0, 0, 0, 1]) 0
    where dfs :: Int32 -> Int32 -> Vector Int32 -> Vector Int32 -> Word8 -> Int32
          dfs result tim amts bots bans
              | tim == 0 = max result geodes
              | upperBd <= result = result
              | otherwise = let (res, bns) = foldl' go (result, bans) $ zip [0..] costs
                            in dfs res (tim-1) (V.zipWith (+) amts bots) bots bns
              where geodes = amts ! 0
                    geodeBots = bots ! 0
                    upperBd = upper (geodes + tim * geodeBots) (amts ! 1) (bots ! 1) (tim-1)
                    obsCost = costs !! 0 ! 1
                    upper ub obs obsRate t
                        | t == 0 = ub
                        | obs >= obsCost = upper (ub + t) (obs + obsRate - obsCost) obsRate (t-1)
                        | otherwise = upper ub (obs + obsRate) (obsRate + 1) (t-1)
                    go (res, bns) (i, cost)
                        | bns .&. shiftL 1 i == 0 &&
                          (i == 0 || bots ! i < maxCosts ! i) &&
                          V.and (V.zipWith (>=) amts cost) =
                              ( dfs res (tim-1) (V.zipWith3 (\a b c -> a + b - c) amts bots cost)
                                        (V.zipWith (+) bots $ V.replicate 4 0 V.// [(i, 1)]) 0
                              , bns .|. shiftL 1 i )
                        | otherwise = (res, bns)

part1 :: String -> Int32
part1 = sum . map (\b@(Blueprint n _ _) -> n * sim 24 b) . blueprints

part2 :: String -> Int32
part2 = product . map (sim 32) . take 3 . blueprints
