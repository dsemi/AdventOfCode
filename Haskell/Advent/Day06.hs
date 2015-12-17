module Advent.Day06
    ( part1
    , part2
    ) where

import Advent.Problem

import Control.Lens
import Control.Monad
import Data.Array.ST
import Data.Array.Unboxed
import Data.Bits
import Data.Either
import Text.ParserCombinators.Parsec

data Action = Off | On | Toggle  deriving (Show)

data Command = Command { cmd :: Action
                       , start :: (Int, Int)
                       , end :: (Int, Int)
                       } deriving (Show)

action :: String -> Action
action "turn off" = Off
action "turn on"  = On
action "toggle"   = Toggle
action _          = undefined

command :: Parser Command
command = do
  a <- try (string "toggle")
       <|> try (string "turn on")
       <|> string "turn off"
  void $ char ' '
  start' <- fmap (both %~ read)
            $ (,) <$> many1 digit <* char ',' <*> many1 digit
  void $ string " through "
  end' <- fmap (both %~ read)
          $ (,) <$> many1 digit <* char ',' <*> many1 digit
  return $ Command (action a) start' end'

runCommands :: (Int -> Int) -> (Int -> Int) -> (Int -> Int)
               -> UArray (Int,Int) Int -> [Command] -> UArray (Int,Int) Int
runCommands f1 f2 f3 grid commands =
  runSTUArray $ do
    arr <- thaw grid
    forM_ commands $ \(Command a (x1, y1) (x2, y2)) -> do
      let f = case a of
                Off    -> f1
                On     -> f2
                Toggle -> f3
      forM_ [x1..x2] $ \x ->
        forM_ [y1..y2] $ \y -> do
          v <- readArray arr (x,y)
          writeArray arr (x,y) $ f v
    return arr

emptyGrid :: UArray (Int, Int) Int
emptyGrid = array ((0,0), (999,999)) [((x,y), 0) | x <- [0..999], y <- [0..999]]

part1 :: Problem
part1 = Pure $ sum . elems . runCommands (const 0) (const 1) (xor 1) emptyGrid
        . rights . map (parse command "") . lines

part2 :: Problem
part2 = Pure $ sum . elems . runCommands f (+1) (+2) emptyGrid
        . rights . map (parse command "") . lines
    where f n = max 0 $ n-1
