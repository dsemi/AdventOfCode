{-# LANGUAGE NamedFieldPuns #-}

module Year2017.Day25
    ( part1
    , part2
    ) where

import Control.Lens
import Data.Either.Utils (fromEither)
import Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as M
import Data.List.PointedList
import Data.Maybe
import Text.Megaparsec
import Text.Megaparsec.Char (anyChar, char, string)
import Text.Megaparsec.Char.Lexer (decimal, signed)


type Tape = PointedList Int
data Rule = Rule Int (Tape -> Tape) Char
data Machine = Machine { tape :: Tape
                       , state :: !Char
                       , rules :: HashMap (Char, Int) Rule
                       }

left :: Tape -> Tape
left t = fromMaybe (insertLeft 0 t) $ previous t

right :: Tape -> Tape
right t = fromMaybe (insertRight 0 t) $ next t

parseMachine :: String -> (Int, Machine)
parseMachine input =
    let Just (steps, start, rules) = parseMaybe parse input
    in (steps, Machine (PointedList [] 0 []) start rules)
    where parse = do
            start <- string "Begin in state " *> anyChar <* string ".\n"
            steps <- string "Perform a diagnostic checksum after " *> int <* string " steps.\n\n"
            rules <- M.fromList . concat <$> parseState `sepBy` string "\n\n"
            return $ (steps, start, rules)
          parseState :: Parsec () String [((Char, Int), Rule)]
          parseState = do
            c <- string "In state " *> anyChar <* string ":\n"
            sequence [parseRule c <* char '\n', parseRule c]
          parseRule :: Char -> Parsec () String ((Char, Int), Rule)
          parseRule c = do
            i <- string "  If the current value is " *> int <* string ":\n"
            v <- string "    - Write the value " *> int <* string ".\n"
            dir <- string "    - Move one slot to the " *> eitherP (string "left." *> pure left)
                                                                   (string "right." *> pure right)
            ns <- string "\n    - Continue with state " *> anyChar <* char '.'
            return $ ((c, i), Rule v (fromEither dir) ns)
          int = signed (return ()) decimal

step :: Int -> Machine -> Machine
step 0 m = m
step n (Machine {tape,state,rules}) =
    let (Rule v f state') = rules ! (state, tape ^. focus)
    in step (n-1) $ Machine (f (replace v tape)) state' rules

part1 :: String -> Int
part1 input = let (n, machine) = parseMachine input
              in sum $ tape $ step n machine

part2 :: String -> String
part2 = const ""
