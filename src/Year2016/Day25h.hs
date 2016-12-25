{-# LANGUAGE TemplateHaskell, StrictData #-}

module Year2016.Day25h where

import Data.Vector (Vector)
import Data.Sequence (Seq)
import Control.Lens.TH (makeLenses)

data Value = Reg Char | Const Int deriving (Eq, Show)

data Instruction = Cpy Value Char
                 | Inc Char
                 | Dec Char
                 | Out Char
                 | Mul Int Char Char Char
                 | Jnz Value Value deriving (Eq, Show)

data Simulator = Sim { _a :: Int
                     , _b :: Int
                     , _c :: Int
                     , _d :: Int
                     , _currentLine :: Int
                     , _output :: Seq Int
                     , _instructions :: Vector Instruction
                     } deriving (Show)

makeLenses ''Simulator
