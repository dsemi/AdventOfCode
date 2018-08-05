{-# LANGUAGE NamedFieldPuns #-}

module Year2017.Day25
    ( part1
    , part2
    ) where

import Utils

import Data.Either.Utils (fromEither)
import Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as M
import Data.Maybe
import Text.Megaparsec (eitherP, parseMaybe, sepBy)
import Text.Megaparsec.Char (anyChar, char, string)
import Text.Megaparsec.Char.Lexer (decimal, signed)


data Rule = Rule Int (Tape -> Tape) Char
data Tape = Tape [Int] Int [Int]
data Machine = Machine { tape :: Tape
                       , state :: !Char
                       , rules :: HashMap (Char, Int) Rule
                       }

left :: Tape -> Tape
left (Tape [] v ys)     = Tape [] 0 (v:ys)
left (Tape (x:xs) v ys) = Tape xs x (v:ys)

right :: Tape -> Tape
right (Tape xs v [])     = Tape (v:xs) 0 []
right (Tape xs v (y:ys)) = Tape (v:xs) y ys

get :: Tape -> Int
get (Tape _ v _) = v

assign :: Int -> Tape -> Tape
assign v (Tape xs _ ys) = Tape xs v ys

parseMachine :: String -> (Int, Machine)
parseMachine input =
    let (steps, start, rules) = fromJust $ parseMaybe parse input
    in (steps, Machine (Tape [] 0 []) start rules)
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
            dir <- string "    - Move one slot to the " *> eitherP (string "left." *> pure left)
                                                                   (string "right." *> pure right)
            ns <- string "\n    - Continue with state " *> anyChar <* char '.'
            return $ ((c, i), Rule v (fromEither dir) ns)
          int = signed (return ()) decimal

step :: Int -> Machine -> Machine
step 0 m = m
step n (Machine {tape,state,rules}) =
    let (Rule v f state') = rules ! (state, get tape)
    in step (n-1) $ Machine (f (assign v tape)) state' rules

part1 :: String -> Int
part1 input = let (n, machine) = parseMachine input
                  (Tape xs v ys) = tape $ step n machine
              in v + sum xs + sum ys

part2 :: String -> String
part2 = const ""
