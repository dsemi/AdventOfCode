{-# LANGUAGE NamedFieldPuns, TemplateHaskell #-}

module Year2015.Day22
    ( part1
    , part2
    ) where

import Control.Lens
import Text.Megaparsec
import Text.Megaparsec.Char (space, string)
import Text.Megaparsec.Char.Lexer (decimal)


data GameState = Game { _pHealth :: Int
                      , _pMana :: Int
                      , _pArmor :: Int
                      , _bHealth :: Int
                      , _bDamage :: Int
                      , _pTurn :: Bool
                      , _hard :: Bool
                      , _effects :: [Effect]
                      }

data ID = M | D | S | P | R deriving (Eq, Show)

type Effect = (ID, [GameState -> GameState])

data Spell = Spell { ident :: ID
                   , cost :: Int
                   , func :: GameState -> GameState
                   }
makeLenses ''GameState

magicMissile :: Spell
magicMissile = Spell M 53 $ bHealth -~ 4

drain :: Spell
drain = Spell D 73 f
    where f = (pHealth +~ 2) . (bHealth -~ 2)

shield :: Spell
shield = Spell S 113 $ effects %~ (es:)
    where es = (S, [pArmor +~ 7, id, id, id, id, pArmor -~ 7])

poison :: Spell
poison = Spell P 173 $ effects %~ (es:)
    where es = (P, replicate 6 $ bHealth -~ 3)

recharge :: Spell
recharge = Spell R 229 $ effects %~ (es:)
    where es = (R, replicate 5 $ pMana +~ 101)

spells :: [Spell]
spells = [ magicMissile, drain, shield, poison, recharge ]

gameOver :: GameState -> Bool
gameOver state = state ^. bHealth <= 0 || state ^. pHealth <= 0

applyEffects :: GameState -> GameState
applyEffects state = foldr ($) (effects %~ filter (not . null . snd) . map (_2 %~ tail) $ state) es
    where es = map (head . snd) $ _effects state

beginTurn :: GameState -> GameState
beginTurn st = (if st ^. hard && st ^. pTurn then pHealth -~ 1 else id) $ applyEffects st

bossAttack :: GameState -> GameState
bossAttack st = pHealth -~ max 1 (st ^. bDamage - st ^. pArmor) $ st

minCostToWin :: GameState -> Int
minCostToWin = minimum . go 0
    where go mana (beginTurn -> state)
              | gameOver state = [ mana | state ^. bHealth <= 0 ]
              | state ^. pTurn =
                  [ v | (Spell {cost, func}) <- [ s | s <- spells
                                                , state ^. pMana >= cost s
                                                , notElem (ident s) . map fst $ state ^. effects
                                                ]
                  , v <- go (mana + cost) $ pTurn %~ not $ pMana -~ cost $ func state
                  ]
              | otherwise = go mana $ pTurn %~ not $ bossAttack state

parseBoss :: Bool -> String -> Maybe GameState
parseBoss hardMode input =
    parseMaybe parser input <&> \(h, d) ->
        Game { _pHealth = 50
             , _pMana = 500
             , _pArmor = 0
             , _bHealth = h
             , _bDamage = d
             , _pTurn = True
             , _hard = hardMode
             , _effects = []
             }
    where int = fromInteger <$> decimal
          parser :: Parsec () String (Int, Int)
          parser = (,) <$>
                   (string "Hit Points: " *> int) <*>
                   (space *> string "Damage: " *> int)

part1 :: String -> Maybe Int
part1 = fmap minCostToWin . parseBoss False

part2 :: String -> Maybe Int
part2 = fmap minCostToWin . parseBoss True
