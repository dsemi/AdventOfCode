{-# LANGUAGE DeriveGeneric, NamedFieldPuns, TemplateHaskell #-}

module Year2015.Day22
    ( part1
    , part2
    ) where

import Control.Lens
import Data.ByteString (ByteString)
import Data.Graph.AStar
import Data.Hashable
import Data.HashSet (HashSet)
import qualified Data.HashSet as S
import Data.Maybe
import FlatParse.Basic
import GHC.Generics (Generic)

data GameState = Game { _pHealth :: Int
                      , _pMana :: Int
                      , _pArmor :: Int
                      , _bHealth :: Int
                      , _bDamage :: Int
                      , _pTurn :: Bool
                      , _hard :: Bool
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

gameOver :: GameState -> Bool
gameOver game = game ^. bHealth <= 0 || game ^. pHealth <= 0

applyEffects :: GameState -> GameState
applyEffects game = foldr ($) (effects %~ (filter (not . null . snd) . map (over _2 tail)) $ game)
                    $ map (applyEffect . head . snd) $ game ^. effects

beginTurn :: GameState -> GameState
beginTurn st = (if st ^. hard && st ^. pTurn then pHealth -~ 1 else id) $ applyEffects st

bossAttack :: GameState -> GameState
bossAttack st = pHealth -~ max 1 (st ^. bDamage - st ^. pArmor) $ st

neighbors :: GameState -> HashSet GameState
neighbors game
    | gameOver game = S.empty
    | game ^. pTurn =
        S.fromList [ beginTurn $ pTurn %~ not $ cast spell game
                   | spell@(Spell id' cost) <- spells
                   , game ^. pMana >= cost
                   , isNothing $ lookup id' (game ^. effects)
                   ]
    | otherwise = S.singleton $ beginTurn $ pTurn %~ not $ bossAttack game

minCostToWin :: GameState -> Maybe Int
minCostToWin game = _spentMana . last <$> aStar neighbors dist (const 0) won (beginTurn game)
    where won g = g ^. bHealth <= 0
          dist g1 g2 = abs (g1 ^. spentMana - g2 ^. spentMana)

parseBoss :: Bool -> ByteString -> Maybe GameState
parseBoss hardMode input =
    case runParser parser input of
      OK (h, d) _ -> Just $ Game { _pHealth = 50
                                 , _pMana = 500
                                 , _pArmor = 0
                                 , _bHealth = h
                                 , _bDamage = d
                                 , _pTurn = True
                                 , _hard = hardMode
                                 , _effects = []
                                 , _spentMana = 0
                                 }
      _ -> Nothing
    where int = anyAsciiDecimalInt
          parser = (,) <$> ($(string "Hit Points: ") *> int) <*> ($(string "\nDamage: ") *> int)

part1 :: ByteString -> Maybe Int
part1 = (>>= minCostToWin) . parseBoss False

part2 :: ByteString -> Maybe Int
part2 = (>>= minCostToWin) . parseBoss True
