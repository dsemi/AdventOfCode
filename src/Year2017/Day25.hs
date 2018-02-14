{-# LANGUAGE NamedFieldPuns #-}

module Year2017.Day25
    ( part1
    , part2
    ) where

import Utils

import Control.Monad.ST
import Data.Either.Utils (fromEither)
import Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as M
import qualified Data.HashTable.Class as H
import qualified Data.HashTable.ST.Basic as B
import Data.Maybe
import Text.Megaparsec (eitherP, parseMaybe, sepBy)
import Text.Megaparsec.Char (anyChar, char, string)
import Text.Megaparsec.Char.Lexer (decimal, signed)


data Rule = Rule { valueToWrite :: Int
                 , dirFun :: Int -> Int
                 , nextState :: Char
                 }
type Tape s = B.HashTable s Int Int
data Machine s = Machine { tape :: Tape s
                         , cursor :: !Int
                         , state :: !Char
                         , rules :: HashMap (Char, Int) Rule
                         }

parseMachine :: String -> ST s (Int, Machine s)
parseMachine input = do
  let (steps, start, rules) = fromJust $ parseMaybe parse input
  tp <- H.new
  pure (steps, Machine tp 0 start rules)
    where parse = do
            start <- string "Begin in state " *> anyChar <* string ".\n"
            steps <- string "Perform a diagnostic checksum after " *> int <* string " steps.\n\n"
            rules <- M.fromList . concat <$> parseState `sepBy` string "\n\n"
            return $ (steps, start, rules)
          parseState :: Parser [((Char, Int), Rule)]
          parseState = do
            c <- string "In state " *> anyChar <* string ":\n"
            sequence [parseRule c <* char '\n', parseRule c]
          parseRule :: Char -> Parser ((Char, Int), Rule)
          parseRule c = do
            i <- string "  If the current value is " *> int <* string ":\n"
            v <- string "    - Write the value " *> int <* string ".\n"
            dir <- string "    - Move one slot to the " *> eitherP (string "left." *> pure pred)
                                                                   (string "right." *> pure succ)
            ns <- string "\n    - Continue with state " *> anyChar <* char '.'
            return $ ((c, i), Rule v (fromEither dir) ns)
          int = signed (return ()) decimal

step :: Int -> Machine s -> ST s Int
step 0 (Machine {tape}) = do
  H.foldM (\a (_, v) -> pure $ a + v) 0 tape
step n (Machine {tape, cursor, state, rules}) = do
  val <- fromMaybe 0 <$> H.lookup tape cursor
  let Rule {valueToWrite, dirFun, nextState} = rules ! (state, val)
  H.insert tape cursor valueToWrite
  step (n-1) $ Machine tape (dirFun cursor) nextState rules

part1 :: String -> Int
part1 input = runST $ parseMachine input >>= uncurry step

part2 :: String -> String
part2 = const ""
