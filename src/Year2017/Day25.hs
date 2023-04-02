{-# LANGUAGE NamedFieldPuns, OverloadedStrings #-}

module Year2017.Day25
    ( part1
    , part2
    ) where

import Data.ByteString (ByteString)
import Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as M
import Data.List.PointedList
import Data.Maybe
import FlatParse.Basic

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

parseMachine :: ByteString -> Maybe (Int, Machine)
parseMachine input = case runParser parse input of
                       OK res _ -> Just res
                       _ -> Nothing
    where parse = do
            start <- $(string "Begin in state ") *> anyAsciiChar <* $(string ".\n")
            steps <- $(string "Perform a diagnostic checksum after ") *> anyAsciiDecimalInt <*
                     $(string " steps.")
            rules <- M.fromList . concat <$> some ($(string "\n\n") >> parseState)
            pure (steps, Machine (PointedList [] 0 []) start rules)
          --parseState :: Parsec () String [((Char, Int), Rule)]
          parseState = do
            c <- $(string "In state ") *> anyAsciiChar <* $(string ":\n")
            sequence [parseRule c <* $(char '\n'), parseRule c]
          --parseRule :: Char -> Parsec () String ((Char, Int), Rule)
          parseRule c = do
            i <- $(string "  If the current value is ") *> anyAsciiDecimalInt <* $(string ":\n")
            v <- $(string "    - Write the value ") *> anyAsciiDecimalInt <* $(string ".\n")
            dir <- $(string "    - Move one slot to the ") *> (($(string "left.") *> pure left) <|>
                                                               ($(string "right.") *> pure right))
            ns <- $(string "\n    - Continue with state ") *> anyAsciiChar <* $(char '.')
            pure ((c, i), Rule v dir ns)

step :: Int -> Machine -> Machine
step 0 m = m
step n (Machine {tape,state,rules}) =
    let Rule v f state' = rules ! (state, _focus tape)
    in step (n-1) $ Machine (f (replace v tape)) state' rules

part1 :: ByteString -> Maybe Int
part1 = fmap (sum . tape . uncurry step) . parseMachine

part2 :: ByteString -> String
part2 = const " "
