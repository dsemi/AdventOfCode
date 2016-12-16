{-# LANGUAGE TemplateHaskell #-}

module UtilsTH where

import Control.Lens (makeLenses)
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)


data AStarState a = AStarState { _openSet :: HashSet a
                               , _closedSet :: HashSet a
                               , _gScore :: HashMap a Int
                               , _fScore :: HashMap a Int
                               }

makeLenses ''AStarState
