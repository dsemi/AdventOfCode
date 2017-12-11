module Year2017.Day07
    ( part1
    , part2
    ) where

import Utils (Parser)

import Control.Monad
import Data.Either (fromLeft)
import Data.Function (on)
import Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as M
import Data.List (groupBy, sortBy)
import Data.Maybe (fromJust, fromMaybe)
import Text.Megaparsec (optional, many, parseMaybe, sepBy)
import Text.Megaparsec.Char (letterChar, string)
import Text.Megaparsec.Char.Lexer (decimal)


data Program = Program { name :: String
                       , weight :: Int
                       , children :: [String]
                       } deriving (Show)

parsePrograms :: String -> [Program]
parsePrograms = map (fromJust . parseMaybe parser) . lines
    where parser :: Parser Program
          parser = do
            name <- many letterChar
            weight <- string " (" *> decimal <* string ")"
            children <- optional $ string " -> " *> many letterChar `sepBy` string ", "
            return $ Program name weight $ fromMaybe [] children

buildInvertedTree :: [Program] -> HashMap String String
buildInvertedTree = foldr f M.empty
    where f (Program parent _ children) m = foldr g m children
              where g child = M.insert child parent

findBottom :: HashMap String String -> String
findBottom m = until (not . (`M.member` m)) (m !) $ head $ M.keys m

part1 :: String -> String
part1 = findBottom . buildInvertedTree . parsePrograms

buildTree :: [Program] -> HashMap String Program
buildTree = foldr f M.empty
    where f p@(Program name _ _) = M.insert name p

findImbalance :: String -> HashMap String Program -> Int
findImbalance root tree = fromLeft 0 $ go $ tree ! root
    where add = fmap $ uncurry (+)
          findAnomaly = head . head . filter ((==1) . length)
                        . groupBy ((==) `on` add)
                        . sortBy (compare `on` add)
          go :: Program -> Either Int (Int, Int)
          go (Program name weight []) = return (weight, 0)
          go (Program name weight children) =
              if all (== expected) totals
              then do
                childSum <- foldr1 (liftM2 (+)) totals
                return (weight, childSum)
              else do
                anomaly <- findAnomaly weights
                expectedTotal <- add $ head $ filter (/= Right anomaly) weights
                Left $ expectedTotal - snd anomaly
              where weights = map (go . (tree !)) children
                    totals@(expected:_) = map add weights

part2 :: String -> Int
part2 input = findImbalance (part1 input) $ buildTree $ parsePrograms input
