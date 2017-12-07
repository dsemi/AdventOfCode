module Year2017.Day07
    ( part1
    , part2
    ) where

import Utils (Parser)

import Data.Either (fromLeft, isLeft, rights)
import Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as M
import Data.List (groupBy, sortBy)
import Data.Maybe (fromJust, fromMaybe)
import Data.Ord (comparing)
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

findImbalance :: String -> HashMap String Program -> Either Int (Int, Int)
findImbalance root tree = go $ tree ! root
    where findAnomaly = head . head . filter ((==1) . length)
                        . groupBy (\(a, b) (c, d) -> a + b == c + d)
                        . sortBy (comparing (uncurry (+)))
          go (Program name weight []) = Right (weight, 0)
          go (Program name weight children)
              | any isLeft childCalcs = head $ filter isLeft childCalcs
              | all (==expected) totals = Right $ (weight, sum $ expected : totals)
              | length weights > 2 =
                  let anomaly = findAnomaly weights
                      expectedTotal = uncurry (+) $ head $ filter (/=anomaly) weights
                  in Left $ expectedTotal - snd anomaly
              | otherwise = undefined
              where childCalcs = map (go . (tree !)) children
                    weights = rights childCalcs
                    (expected:totals) = map (uncurry (+)) $ weights

part2 :: String -> Int
part2 input = let root = part1 input
              in fromLeft 0 $ findImbalance root $ buildTree $ parsePrograms input
