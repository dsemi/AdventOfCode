module Year2015.Day09
    ( part1
    , part2
    ) where

import Utils

import Data.HashMap.Strict ((!))
import qualified Data.HashMap.Strict as M
import Data.List (permutations)
import Data.Maybe
import Text.Megaparsec (parseMaybe, some)
import Text.Megaparsec.Char (alphaNumChar, string)
import Text.Megaparsec.Char.Lexer (decimal)


data Edge = Edge String String Int

opposite :: Edge -> Edge
opposite (Edge p1 p2 d) = Edge p2 p1 d

parseLine :: String -> Edge
parseLine = fromJust . parseMaybe parser
    where parser :: Parser Edge
          parser = Edge <$> some alphaNumChar <* string " to "
                        <*> some alphaNumChar <* string " = "
                        <*> (fromInteger <$> decimal)

allPathDistances :: [String] -> [Int]
allPathDistances input = let m = constructMap $ map parseLine input
                             paths = permutations $ M.keys m
                         in map (\p -> sum . zipWith (\a b -> m ! a ! b) p $ tail p) paths
    where constructMap = foldr (\e -> addEdgeToMap (opposite e) . addEdgeToMap e) M.empty
          addEdgeToMap (Edge p1 p2 d) m = let m' = M.lookupDefault M.empty p1 m
                                          in M.insert p1 (M.insert p2 d m') m

part1 :: String -> Int
part1 = minimum . allPathDistances . lines

part2 :: String -> Int
part2 = maximum . allPathDistances . lines
