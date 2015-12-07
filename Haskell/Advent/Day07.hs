module Advent.Day07
    ( part1
    , part2
    ) where

import Control.Monad
import Data.Bits
import Data.Either
import Data.Function.Memoize
import Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as M
import Data.Tuple
import Text.ParserCombinators.Parsec

data Atom = Value Int | Var String deriving (Show)

data Node = Singleton Atom
          | Not Atom
          | And Atom Atom
          | Or Atom Atom
          | LShift Atom Atom
          | RShift Atom Atom deriving (Show)

eval :: HashMap String Node -> String -> Int
eval m = me
    where e :: String -> Int
          e k = case m ! k of
                  (Singleton a)  -> getAtom a
                  (Not a)        -> complement $ getAtom a
                  (And a1 a2)    -> getAtom a1 .&. getAtom a2
                  (Or a1 a2)     -> getAtom a1 .|. getAtom a2
                  (LShift a1 a2) -> getAtom a1 `shiftL` getAtom a2
                  (RShift a1 a2) -> getAtom a1 `shiftR` getAtom a2
          me = memoize e
          getAtom (Value i) = i
          getAtom (Var s)   = me s

readData :: [String] ->  HashMap String Node
readData mappings = M.fromList . rights $ map (parse parseLine "") mappings
    where parseLine = fmap swap $ (,) <$> parseNode <* string " -> " <*> many1 letter
          parseNode = try parseNot <|> try parseAnd <|> try parseOr
                      <|> try parseLShift <|> try parseRShift
                      <|> parseSingleton
          parseAtom = try parseValue <|> parseVar
          parseValue = Value . read <$> many1 digit
          parseVar = Var <$> many1 letter
          parseSingleton = Singleton <$> parseAtom
          parseNot = Not <$> (string "NOT " *> parseAtom)
          parseAnd = And <$> parseAtom <* string " AND " <*> parseAtom
          parseOr = Or <$> parseAtom <* string " OR " <*> parseAtom
          parseLShift = LShift <$> parseAtom <* string " LSHIFT " <*> parseAtom
          parseRShift = RShift <$> parseAtom <* string " RSHIFT " <*> parseAtom

part1 :: String -> String
part1 = show . (`eval` "a") . readData . lines

part2 :: String -> String
part2 input = let wiring = readData $ lines input
                  ans1 = eval wiring "a"
                  wiring' = M.insert "b" (Singleton $ Value ans1) wiring
              in show $ eval wiring' "a"
