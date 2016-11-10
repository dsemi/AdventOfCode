{-# LANGUAGE TemplateHaskell #-}

module Year2015.Day22h where

import Control.Lens.TH

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
