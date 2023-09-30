{-# LANGUAGE DeriveGeneric, NamedFieldPuns, TemplateHaskell #-}

module Year2015.Day22
    ( part1
    , part2
    ) where

import Control.Lens
import Data.ByteString (ByteString)
import Data.Hashable
import Data.List (find)
import Data.Maybe
import FlatParse.Basic
import GHC.Generics (Generic)

import Utils (dijkstra)

data GameState = Game { _pHealth :: Int
                      , _pMana :: Int
                      , _pArmor :: Int
                      , _bHealth :: Int
                      , _bDamage :: Int
                      , _effects :: [(ID, [Effect])]
                      , _spentMana :: Int
                      } deriving (Eq, Ord, Generic)
instance Hashable GameState

data ID = MagicMissile
        | Drain
        | Shield
        | Poison
        | Recharge
          deriving (Eq, Ord, Generic)
instance Hashable ID

data Effect = AddArmor
            | RemoveArmor
            | Poison'
            | Recharge'
            | Nop
              deriving (Eq, Ord, Generic)
instance Hashable Effect

makeLenses ''GameState

data Spell = Spell ID Int
spells :: [Spell]
spells = [ Spell MagicMissile 53, Spell Drain 73
         , Spell Shield 113, Spell Poison 173, Spell Recharge 229 ]

cast :: Spell -> GameState -> GameState
cast (Spell id' cost) = (spentMana +~ cost) . (pMana -~ cost) . f id'
    where f MagicMissile = bHealth -~ 4
          f Drain = (pHealth +~ 2) . (bHealth -~ 2)
          f Shield = effects %~ ((Shield, [AddArmor, Nop, Nop, Nop, Nop, RemoveArmor]) :)
          f Poison = effects %~ ((Poison, (replicate 6 Poison')) :)
          f Recharge = effects %~ ((Recharge, (replicate 5 Recharge')) :)

applyEffect :: Effect -> GameState -> GameState
applyEffect AddArmor = pArmor +~ 7
applyEffect RemoveArmor = pArmor -~ 7
applyEffect Poison' = bHealth -~ 3
applyEffect Recharge' = pMana +~ 101
applyEffect Nop = id

applyEffects :: GameState -> GameState
applyEffects game = foldr ($) (effects %~ (filter (not . null . snd) . map (over _2 tail)) $ game)
                    $ map (applyEffect . head . snd) $ game ^. effects

bossAttack :: GameState -> GameState
bossAttack st = pHealth -~ max 1 (st ^. bDamage - st ^. pArmor) $ st

neighbors :: Bool -> GameState -> [(Int, GameState)]
neighbors hard game
    | hard && game ^. pHealth <= 1 = []
    | st ^. bHealth <= 0 = [(0, st)]
    | otherwise =
        [ (cost, st')
        | spell@(Spell id' cost) <- spells
        , st ^. pMana >= cost
        , isNothing $ lookup id' (st ^. effects)
        , let st' = bossAttack $ applyEffects $ cast spell st
        , st' ^. bHealth <= 0 || st' ^. pHealth > 0
        ]
    where st = applyEffects $ (if hard then pHealth -~ 1 else id) game

minCostToWin :: Bool -> GameState -> Maybe Int
minCostToWin hard game = fmap fst $ find (\(_, s) -> s ^. bHealth <= 0) $ dijkstra game (neighbors hard)

parseBoss :: ByteString -> Maybe GameState
parseBoss input =
    case runParser parser input of
      OK (h, d) _ -> Just $ Game { _pHealth = 50
                                 , _pMana = 500
                                 , _pArmor = 0
                                 , _bHealth = h
                                 , _bDamage = d
                                 , _effects = []
                                 , _spentMana = 0
                                 }
      _ -> Nothing
    where int = anyAsciiDecimalInt
          parser = (,) <$> ($(string "Hit Points: ") *> int) <*> ($(string "\nDamage: ") *> int)

part1 :: ByteString -> Maybe Int
part1 = (>>= minCostToWin False) . parseBoss

part2 :: ByteString -> Maybe Int
part2 = (>>= minCostToWin True) . parseBoss
