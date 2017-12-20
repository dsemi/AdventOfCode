module Year2015.Day21
    ( part1
    , part2
    ) where

import Utils

import Data.List (foldl1', partition, tails)
import Data.Maybe
import Text.Megaparsec (parseMaybe)
import Text.Megaparsec.Char (space, string)
import Text.Megaparsec.Char.Lexer (decimal)


combinations :: [a] -> Int -> [[a]]
combinations  _ 0 = [[]]
combinations xs n = [ y:ys | y:xs' <- tails xs
                    , ys <- combinations xs' $ n-1 ]

data Person = Person { hitpoints :: Int
                     , cost :: Int
                     , damage :: Int
                     , armor :: Int
                     }

person :: [Int] -> Person
person [c, d, a] = Person 100 c d a
person _ = error "Invalid person"

shop1 :: [(String, [Int])]
shop1 = [ ("Dagger", [8, 4, 0]), ("Shortsword", [10, 5, 0])
        , ("Warhammer", [25, 6, 0]), ("Longsword", [40, 7, 0])
        , ("Greataxe", [74, 8, 0]) ]

shop2 :: [(String, [Int])]
shop2 = [ ("Leather", [13, 0, 1]), ("Chainmail", [31, 0, 2])
        , ("Splintmail", [53, 0, 3]), ("Bandedmail", [75, 0, 4])
        , ("Platemail", [102, 0, 5]) ]

shop3 :: [(String, [Int])]
shop3 = [ ("Damage +1", [25, 1, 0]), ("Damage +2", [50, 2, 0])
        , ("Damage +3", [100, 3, 0]), ("Defense +1", [20, 0, 1])
        , ("Defense +2", [40, 0, 2]), ("Defense +3", [80, 0, 3])
        , ("None", [0, 0, 0])]

parseBoss :: String -> Person
parseBoss = fromJust . parseMaybe parser
    where int = fromInteger <$> decimal
          parser :: Parser Person
          parser = do
            hp <- string "Hit Points: " *> int
            space
            d <- string "Damage: " *> int
            space
            a <- string "Armor: " *> int
            return $ Person hp 0 d a


allEquipmentCombos :: [Person]
allEquipmentCombos = [ person $ foldl1' (zipWith (+)) [weapon, armor', rings]
                     | weapon <- map snd shop1
                     , armor' <- map snd shop2
                     , combo <- combinations (map snd shop3) 2
                     , rings <- map (foldl1' (zipWith (+))) . init $ tails combo ]

isWinning :: Person -> Person -> Bool
isWinning b p = playerTurnsToDie >= bossTurnsToDie
    where bossTurnsToDie = ttd b p
          playerTurnsToDie = ttd p b
          ttd p1 p2
              | r == 0    = q
              | otherwise = q + 1
              where (q, r) = hitpoints p1 `quotRem` max 1 (damage p2 - armor p1)

allBattles :: Person -> ([Person], [Person])
allBattles boss = partition (isWinning boss) allEquipmentCombos

part1 :: String -> Int
part1 = minimum . map cost . fst . allBattles . parseBoss

part2 :: String -> Int
part2 = maximum . map cost . snd . allBattles . parseBoss
