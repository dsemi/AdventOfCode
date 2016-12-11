module Year2015.Day06
    ( part1
    , part2
    ) where

import Control.Monad
import Data.Array.ST
import Data.Array.Unboxed
import Data.Bits
import Data.Maybe
import Text.Megaparsec
import Text.Megaparsec.Lexer
import Text.Megaparsec.String

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

command :: String -> Command
command = fromJust . parseMaybe parser
    where parseAction = action <$> choice (map string ["toggle", "turn off", "turn on"])
          parseIntTuple :: Parser (Int, Int)
          parseIntTuple = (,) <$> (fromInteger <$> integer <* char ',')
                              <*> (fromInteger <$> integer)
          parser :: Parser Command
          parser = Command <$> parseAction <* spaceChar <*> parseIntTuple
                           <* string " through " <*> parseIntTuple

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

part1 :: String -> String
part1 = show . sum . elems . runCommands (const 0) (const 1) (xor 1) emptyGrid
        . map command . lines

part2 :: String -> String
part2 = show . sum . elems . runCommands f (+1) (+2) emptyGrid
        . map command . lines
    where f n = max 0 $ n-1