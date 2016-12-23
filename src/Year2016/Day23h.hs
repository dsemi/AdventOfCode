{-# LANGUAGE TemplateHaskell, StrictData #-}

module Year2016.Day23h where

import Data.Vector (Vector)
import Control.Lens.TH (makeLenses)

data Value = Reg Char | Const Int deriving (Eq, Show)

data Instruction = Cpy Value Char
                 | Inc Char
                 | Dec Char
                 | Tgl Char
                 | Jnz Value Value
                 | Noop (Maybe Instruction)
                 | Mul Char Char Char Char deriving (Eq, Show)

data Simulator = Sim { _a :: Int
                     , _b :: Int
                     , _c :: Int
                     , _d :: Int
                     , _currentLine :: Int
                     , _instructions :: Vector Instruction
                     } deriving (Show)

makeLenses ''Simulator
