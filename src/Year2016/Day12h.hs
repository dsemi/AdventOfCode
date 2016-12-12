{-# LANGUAGE TemplateHaskell #-}

module Year2016.Day12h where

import Data.Vector (Vector)
import Control.Lens.TH

data Instruction = Cpy (Either Char Int) Char
                 | Inc Char
                 | Dec Char
                 | Jnz (Either Char Int) Int deriving (Show)

data Simulator = Sim { _a :: Int
                     , _b :: Int
                     , _c :: Int
                     , _d :: Int
                     , _currentLine :: Int
                     , _instrs :: Vector Instruction
                     }

makeLenses ''Simulator
