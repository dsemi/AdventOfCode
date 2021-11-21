module Year2015.Day06
    ( part1
    , part2
    ) where

import Control.Monad
import Control.Monad.ST
import Data.Bits
import Data.Maybe
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import Text.Megaparsec
import Text.Megaparsec.Char (char, spaceChar, string)
import Text.Megaparsec.Char.Lexer (decimal)


data Action = Off | On | Toggle deriving (Show)

data Command = Command Action (Int, Int) (Int, Int) deriving (Show)

action :: String -> Action
action "turn off" = Off
action "turn on"  = On
action "toggle"   = Toggle
action _ = error "Invalid string"

command :: String -> Command
command = fromJust . parseMaybe parser
    where parseAction = action <$> choice (map string ["toggle", "turn off", "turn on"])
          int = fromInteger <$> decimal
          pair :: Parsec () String (Int, Int)
          pair = (,) <$> (int <* char ',') <*> int
          parser :: Parsec () String Command
          parser = Command <$> parseAction <* spaceChar <*> pair
                           <* string " through " <*> pair

runCommands :: (Int -> Int) -> (Int -> Int) -> (Int -> Int)
            -> [Command] -> Vector Int
runCommands f1 f2 f3 commands =
  runST $ do
    arr <- MV.new 1000000
    forM_ commands $ \(Command a (x1, y1) (x2, y2)) ->
        let f = case a of
                  Off    -> f1
                  On     -> f2
                  Toggle -> f3
        in forM_ [x1..x2] $ \x ->
            let row = 1000 * x
            in forM_ [row + y1..row + y2] $ MV.unsafeModify arr f
    V.unsafeFreeze arr

part1 :: String -> Int
part1 = V.sum . runCommands (const 0) (const 1) (xor 1)
        . map command . lines

part2 :: String -> Int
part2 = V.sum . runCommands (max 0 . subtract 1) (+1) (+2)
        . map command . lines
