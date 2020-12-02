module Year2015.Day21
    ( part1
    , part2
    ) where

import Utils

import Data.Bool
import Data.List (tails)
import Data.Maybe
import Text.Megaparsec
import Text.Megaparsec.Char.Lexer (decimal)


data Equip = Equip { cost :: Int, _damage :: Int, _armor :: Int}

data Person = Person { _hitpoints :: Int, equip :: Equip }

add :: Equip -> Equip -> Equip
add (Equip c1 d1 a1) (Equip c2 d2 a2) =
    Equip (c1 + c2) (d1 + d2) (a1 + a2)

weapons :: [Equip]
weapons = [ Equip 8 4 0 -- Dagger
          , Equip 10 5 0 -- Shortsword
          , Equip 25 6 0 -- Warhammer
          , Equip 40 7 0 -- Longsword
          , Equip 74 8 0 -- Greataxe
          ]

armors :: [Equip]
armors = [ Equip 13 0 1 -- Leather
         , Equip 31 0 2 -- Chainmail
         , Equip 53 0 3 -- Splintmail
         , Equip 75 0 4 -- Bandedmail
         , Equip 102 0 5 -- Platemail
         ]

rings :: [Equip]
rings = [ Equip 25 1 0 -- Damage +1
        , Equip 50 2 0 -- Damage +2
        , Equip 100 3 0 -- Damage +3
        , Equip 20 0 1 -- Defense +1
        , Equip 40 0 2 -- Defense +2
        , Equip 80 0 3 -- Defense +3
        , Equip 0 0 0 -- None
        ]

parseBoss :: String -> Person
parseBoss = fromJust . parseMaybe parser
    where int = fromInteger <$> decimal
          parser :: Parsec () String Person
          parser = do
            hp <- chunk "Hit Points: " *> int <* chunk "\n"
            d <- chunk "Damage: " *> int <* chunk "\n"
            a <- chunk "Armor: " *> int
            pure $ Person hp $ Equip 0 d a

allEquipCombos :: [Person]
allEquipCombos = [ Person 100 $ foldl1 add [weapon, armor', rings']
                 | (weapon, armor') <- (,) <$> weapons <*> armors
                 , combo <- combinations rings 2
                 , rings' <- map (foldl1 add) . init $ tails combo ]

isWinning :: Person -> Person -> Bool
isWinning b p = ttd p b >= ttd b p
    where ttd (Person hp (Equip _ _ a)) (Person _ (Equip _ d _)) = bool (q+1) q $ r == 0
              where (q, r) = hp `quotRem` max 1 (d - a)

part1 :: String -> Int
part1 = minimum . map (cost . equip) . (`filter` allEquipCombos) . isWinning . parseBoss

part2 :: String -> Int
part2 = maximum . map (cost . equip) . (`filter` allEquipCombos) . (not .) . isWinning . parseBoss
