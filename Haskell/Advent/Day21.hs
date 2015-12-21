{-# LANGUAGE QuasiQuotes, FlexibleContexts #-}

module Advent.Day21
    ( part1
    , part2
    ) where

import Advent.Problem

import Data.List (foldl1', partition, tails)
import Data.Tuple.HT
import Text.Regex.PCRE.Heavy

combinations :: [a] -> Int -> [[a]]
combinations  _ 0 = [[]]
combinations xs n = [ y:ys | y:xs' <- tails xs
                    , ys <- combinations xs' $ n-1 ]

lToT [a, b, c] = (a, b, c)

data Person = Person { hp :: Int
                     , cost :: Int
                     , damage :: Int
                     , armor :: Int
                     }

shop1 = [ ("Dagger", [8, 4, 0]), ("Shortsword", [10, 5, 0])
        , ("Warhammer", [25, 6, 0]), ("Longsword", [40, 7, 0])
        , ("Greataxe", [74, 8, 0]) ]

shop2 = [ ("Leather", [13, 0, 1]), ("Chainmail", [31, 0, 2])
        , ("Splintmail", [53, 0, 3]), ("Bandedmail", [75, 0, 4])
        , ("Platemail", [102, 0, 5]) ]

shop3 = [ ("Damage+1", [25, 1, 0]), ("Damage+2", [50, 2, 0])
        , ("Damage+3", [100, 3, 0]), ("Defense+1", [20, 0, 1])
        , ("Defense+2", [40, 0, 2]), ("Defense+3", [80, 0, 3])
        , ("None", [0, 0, 0])]

parseBoss :: String -> Person
parseBoss = uncurry3 (\h d a -> Person h 0 d a) . lToT
            . map read . snd . head . scan regex
    where regex = [redotall|Hit Points: (\d+).*Damage: (\d+).*Armor: (\d+)|]

allEquipmentCombos = [ uncurry3 (Person 100) . lToT
                     $ foldl1' (zipWith (+)) [weapon, armor, rings]
                     | weapon <- map snd shop1
                     , armor <- map snd shop2
                     , combo <- combinations (map snd shop3) 2
                     , rings <- map (foldl1' (zipWith (+))) . init $ tails combo ]

isWinning b p = playerTurnsToDie >= bossTurnsToDie
    where ttd p1 p2 = ceiling $ fromIntegral (hp p1)
                      / max 1 (fromIntegral (damage p2) - fromIntegral (armor p1))
          bossTurnsToDie = ttd b p
          playerTurnsToDie = ttd p b

allBattles boss = partition (isWinning boss) allEquipmentCombos

part1 :: Problem
part1 = Pure $ minimum . map cost . fst . allBattles . parseBoss

part2 :: Problem
part2 = Pure $ maximum . map cost . snd . allBattles . parseBoss
