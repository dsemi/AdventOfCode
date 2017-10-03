{-# LANGUAGE TemplateHaskell #-}

module Year2015.Day22
    ( part1
    , part2
    ) where

import Utils

import Control.Lens
import Control.Lens.TH
import Text.Megaparsec (parseMaybe)
import Text.Megaparsec.Char (space, string)
import Text.Megaparsec.Char.Lexer (decimal)


data GameState = Game { _pHealth :: Int
                      , _pMana :: Int
                      , _pArmor :: Int
                      , _bHealth :: Int
                      , _bDamage :: Int
                      , _effects :: [Effect]
                      }

data ID = M | D | S | P | R deriving (Eq, Show)

type Effect = (ID, [GameState -> GameState])

data Spell = SingleSpell { _id :: ID
                         , cost :: Int
                         , func :: GameState -> GameState
                         }
           | EffectSpell { _id :: ID
                         , cost :: Int
                         , effect :: Effect
                         }

makeLenses ''GameState

data EndGame = PlayerWon Int | PlayerLost Int

won (PlayerWon _) = True
won _             = False

int (PlayerWon  i) = i
int (PlayerLost i) = i

ints = map int

magicMissile = SingleSpell M 53 $ bHealth -~ 4

drain = SingleSpell D 73 f
    where f = (pHealth +~ 2) . (bHealth -~ 2)

shield = EffectSpell S 113 es
    where es = (S, [pArmor +~ 7, id, id, id, id, pArmor -~ 7])

poison = EffectSpell P 173 es
    where es = (P, replicate 6 $ bHealth -~ 3)

recharge = EffectSpell R 229 es
    where es = (R, replicate 5 $ pMana +~ 101)

spells = [ magicMissile, drain, shield, poison, recharge ]

gameOver state = state ^. bHealth <= 0 || state ^. pHealth <= 0

applyEffects state = foldr ($) (effects %~ filter (not . null . snd) . map (_2 %~ tail) $ state) es
    where es = map (head . snd) $ _effects state

turn hard state m pt
    | gameOver state' = if _bHealth state' <= 0
                        then [PlayerWon m]
                        else [PlayerLost m]
    | pt              = if null playerStates -- Player can't choose any spells
                        then [PlayerLost m]
                        else playerStates
    | otherwise       = turn hard (pHealth -~ max 1 (_bDamage state' - _pArmor state') $ state') m
                        $ not pt
    where state' = (if hard && pt then pHealth -~ 1 else id) $ applyEffects state
          playerStates =
              [ endState
              | spell <- [ s | s <- spells
                         , state' ^. pMana >= cost s
                         , notElem (_id s) . map fst $ state' ^. effects
                         ]
              , let state'' = case spell of
                                (SingleSpell _ c f) -> pMana -~ c $ f state'
                                (EffectSpell _ c e) -> pMana -~ c $ effects %~ (e:) $ state'
              , endState <- turn hard state'' (m + cost spell) $ not pt
              ]

parseBoss :: String -> GameState
parseBoss input = let (Just (h, d)) = parseMaybe parser input
                  in Game { _pHealth = 50
                          , _pMana = 500
                          , _pArmor = 0
                          , _bHealth = h
                          , _bDamage = d
                          , _effects = []
                          }
    where int = fromInteger <$> decimal
          parser :: Parser (Int, Int)
          parser = do
            h <- string "Hit Points: " *> int
            space
            d <- string "Damage: " *> int
            return (h, d)


part1 :: String -> Int
part1 input = minimum . ints . filter won $ turn False (parseBoss input) 0 True

part2 :: String -> Int
part2 input = minimum . ints . filter won $ turn True (parseBoss input) 0 True
