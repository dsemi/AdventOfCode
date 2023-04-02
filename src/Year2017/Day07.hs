{-# LANGUAGE NamedFieldPuns, OverloadedStrings #-}

module Year2017.Day07
    ( part1
    , part2
    ) where

import Control.Arrow ((&&&))
import Control.Monad (liftM2)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Either (fromLeft)
import Data.Function (on)
import qualified Data.HashSet as S
import Data.List (groupBy, sortBy, transpose)
import Data.Maybe (fromMaybe)
import Data.Tree (Tree(..), unfoldTreeM)
import FlatParse.Basic

import Utils

part1 :: ByteString -> ByteString
part1 input = let [as, bs] = transpose $ map (splitOn " -> ") $ B.lines input
              in foldr1 const $ S.fromList (map (head . B.words) as)
                     `S.difference` S.fromList (concatMap (splitOn ", ") bs)

data Program = Program { name :: ByteString
                       , weight :: Int
                       } deriving (Show)

parsePrograms :: ByteString -> [(Program, [ByteString])]
parsePrograms = map parse . B.lines
    where word = byteStringOf $ some $ satisfy isLatinLetter
          parser = do
            name' <- word <* $(char ' ')
            weight' <- $(char '(') *> anyAsciiDecimalInt <* $(char ')')
            children <- optional $ $(string " -> ") *> some (word <* optional_ $(string ", "))
            return (Program name' weight', fromMaybe [] children)
          parse line = case runParser parser line of
                         OK res _ -> res
                         _ -> error "unreachable"

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

part2 :: ByteString -> Int
part2 input = let programs = map (name . fst &&& id) $ parsePrograms input
                  root = part1 input
                  Just tree = unfoldTreeM (`lookup` programs) root
              in fromLeft 0 $ findImbalance tree
