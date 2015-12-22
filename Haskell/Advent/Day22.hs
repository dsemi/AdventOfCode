{-# LANGUAGE QuasiQuotes #-}

module Advent.Day22
    ( part1
    , part2
    ) where

import Advent.Day22h
import Advent.Problem

import Control.Lens
import Text.Regex.PCRE.Heavy

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
parseBoss input = let [h, d] = map read . snd . head $ scan regex input
                  in Game { _pHealth = 50
                          , _pMana = 500
                          , _pArmor = 0
                          , _bHealth = h
                          , _bDamage = d
                          , _effects = []
                          }
    where regex = [redotall|Hit Points: (\d+).*Damage: (\d+)|]


part1 :: Problem
part1 = Pure f
    where f input = minimum . ints . filter won $ turn False (parseBoss input) 0 True

part2 :: Problem
part2 = Pure f
    where f input = minimum . ints . filter won $ turn True (parseBoss input) 0 True
