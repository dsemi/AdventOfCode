module Year2015.Day06
    ( part1
    , part2
    ) where

import Control.Monad
import Control.Monad.ST
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import FlatParse.Basic

data Action = Off | On | Toggle deriving (Show)

data Command = Command Action (Int, Int) (Int, Int) deriving (Show)

command :: ByteString -> Command
command s = case runParser parser s of
              OK cmd _ -> cmd
              _ -> error "unreachable"
    where parseAction = ($(string "toggle") *> pure Toggle)
                        <|> ($(string "turn off") *> pure Off)
                        <|> ($(string "turn on") *> pure On)
          pair = (,) <$> (anyAsciiDecimalInt <* $(char ',')) <*> anyAsciiDecimalInt
          parser = Command <$> parseAction <* $(char ' ') <*> pair
                           <* $(string " through ") <*> pair

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

part1 :: ByteString -> Int
part1 = V.sum . runCommands (const 0) (const 1) (xor 1)
        . map command . B.lines

part2 :: ByteString -> Int
part2 = V.sum . runCommands (max 0 . subtract 1) (+1) (+2)
        . map command . B.lines
