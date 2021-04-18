{-# LANGUAGE NamedFieldPuns, OverloadedStrings #-}

module Year2017.Day25
    ( part1
    , part2
    ) where

import Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as M
import Data.List.PointedList
import Data.Maybe
import Text.Megaparsec
import Text.Megaparsec.Char.Lexer (decimal)


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

parseMachine :: String -> Maybe (Int, Machine)
parseMachine = parseMaybe $ do
                 start <- "Begin in state " *> anySingle <* ".\n"
                 steps <- "Perform a diagnostic checksum after " *> decimal <* " steps.\n\n"
                 rules <- M.fromList . concat <$> parseState `sepBy` "\n\n"
                 pure (steps, Machine (PointedList [] 0 []) start rules)
    where parseState :: Parsec () String [((Char, Int), Rule)]
          parseState = do
            c <- "In state " *> anySingle <* ":\n"
            sequence [parseRule c <* "\n", parseRule c]
          parseRule :: Char -> Parsec () String ((Char, Int), Rule)
          parseRule c = do
            i <- "  If the current value is " *> decimal <* ":\n"
            v <- "    - Write the value " *> decimal <* ".\n"
            dir <- "    - Move one slot to the " *> (("left." *> pure left) <|>
                                                     ("right." *> pure right))
            ns <- "\n    - Continue with state " *> anySingle <* "."
            pure ((c, i), Rule v dir ns)

step :: Int -> Machine -> Machine
step 0 m = m
step n (Machine {tape,state,rules}) =
    let Rule v f state' = rules ! (state, _focus tape)
    in step (n-1) $ Machine (f (replace v tape)) state' rules

part1 :: String -> Maybe Int
part1 = fmap (sum . tape . uncurry step) . parseMachine

part2 :: String -> String
part2 = const ""
