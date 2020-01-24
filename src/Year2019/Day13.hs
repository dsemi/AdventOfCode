{-# LANGUAGE FlexibleContexts, TemplateHaskell #-}

module Year2019.Day13
    ( part1
    , part2
    ) where

import Control.Lens
import Control.Monad
import Control.Monad.State
import qualified Data.IntMap as M
import Linear.V2
import Pipes
import qualified Pipes.Prelude as P

import Year2019.IntCode


data Tile = Empty | Wall | Block | Paddle | Ball deriving (Enum, Eq)

data Instr = Draw (V2 Int) Tile | Score Int

parseInstrs :: (Monad m) => Pipe Int Instr m r
parseInstrs = forever $ liftM3 parseInstr await await await >>= yield
    where parseInstr (-1) 0 score = Score score
          parseInstr x y tile = Draw (V2 x y) $ toEnum tile

part1 :: String -> Identity Int
part1 input = P.length $ pure () >-> runProg (parse input) >-> parseInstrs >-> P.filter isBlock
    where isBlock (Draw _ Block) = True
          isBlock _ = False

data Game = Game { _ballX :: Int
                 , _paddleX :: Int
                 , _score :: Int
                 }
makeLenses ''Game

play :: (MonadState Game m) => Consumer Instr m ()
play = forever $ await >>= \case
       (Draw (V2 x _) Ball) -> ballX .= x
       (Draw (V2 x _) Paddle) -> paddleX .= x
       (Score num) -> score .= num
       _ -> pure ()

-- Move paddle to always follow the ball
controller :: (MonadState Game m) => Producer Int m ()
controller = forever $ liftM2 compare (use ballX) (use paddleX) >>= yield . pred . fromEnum

part2 :: String -> Int
part2 input = view score $ flip execState (Game 0 0 0) $ runEffect
              $ controller >-> runProg (M.insert 0 2 $ parse input) >-> parseInstrs >-> play
