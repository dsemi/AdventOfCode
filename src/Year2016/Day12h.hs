{-# LANGUAGE TemplateHaskell #-}

module Year2016.Day12h where

import Data.Vector (Vector)
import Control.Lens.TH (makeLenses)

data Value = Reg Char | Const Int deriving (Show)

data Instruction = Cpy Value Char
                 | Inc Char
                 | Dec Char
                 | Jnz Value Int deriving (Show)

data Simulator = Sim { _a :: Int
                     , _b :: Int
                     , _c :: Int
                     , _d :: Int
                     , _currentLine :: Int
                     , _instructions :: Vector Instruction
                     }

makeLenses ''Simulator
