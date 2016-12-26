{-# LANGUAGE TemplateHaskell, StrictData #-}

module Year2016.AssembunnyTH where

import Data.Sequence (Seq)
import Data.Vector (Vector)
import Control.Lens.TH

data Value = Reg Char | Const Int deriving (Eq, Show)

data Instruction = Cpy Value Value
                 | Inc Char
                 | Dec Char
                 | Tgl Char
                 | Out Char
                 | Jnz Value Value
                 | Mul Value Char Char Char deriving (Eq, Show)

data Simulator = Sim { _a :: Int
                     , _b :: Int
                     , _c :: Int
                     , _d :: Int
                     , _currentLine :: Int
                     , _outputLim :: Int
                     , _output :: Seq Int
                     , _instructions :: Vector Instruction
                     } deriving (Eq, Show)

makeLenses ''Simulator
