{-# LANGUAGE NamedFieldPuns #-}

module Year2017.Day07
    ( part1
    , part2
    ) where

import Control.Arrow ((&&&))
import Control.Monad (liftM2)
import Data.Either (fromLeft)
import Data.Function (on)
import qualified Data.HashSet as S
import Data.List (groupBy, sortBy, transpose)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust, fromMaybe)
import Data.Tree (Tree(..), unfoldTreeM)
import Text.Megaparsec
import Text.Megaparsec.Char (letterChar, string)
import Text.Megaparsec.Char.Lexer (decimal)


part1 :: String -> String
part1 input = let [as, bs] = transpose $ map (splitOn " -> ") $ lines input
              in foldr1 const $ S.fromList (map (head . words) as)
                     `S.difference` S.fromList (concatMap (splitOn ", ") bs)

data Program = Program { name :: String
                       , weight :: Int
                       } deriving (Show)

parsePrograms :: String -> [(Program, [String])]
parsePrograms = map (fromJust . parseMaybe parser) . lines
    where parser :: Parsec () String (Program, [String])
          parser = do
            name' <- many letterChar
            weight' <- string " (" *> decimal <* string ")"
            children <- optional $ string " -> " *> many letterChar `sepBy` string ", "
            return (Program name' weight', fromMaybe [] children)

findImbalance :: Tree Program -> Either Int (Int, Int)
findImbalance (Node (Program {weight}) []) = return (weight, 0)
findImbalance (Node (Program {weight}) children) = do
  let weights = map findImbalance children
      totals@(expected:_) = map add weights
  if all (== expected) totals
  then do
    childSum <- foldr1 (liftM2 (+)) totals
    pure (weight, childSum)
  else do
    anomaly <- findAnomaly weights
    expectedTotal <- add $ head $ filter (/= Right anomaly) weights
    Left $ expectedTotal - snd anomaly
    where add = fmap $ uncurry (+)
          findAnomaly = head . head . filter ((==1) . length)
                        . groupBy ((==) `on` add)
                        . sortBy (compare `on` add)

part2 :: String -> Int
part2 input = let programs = map (name . fst &&& id) $ parsePrograms input
                  root = part1 input
                  Just tree = unfoldTreeM (`lookup` programs) root
              in fromLeft 0 $ findImbalance tree
