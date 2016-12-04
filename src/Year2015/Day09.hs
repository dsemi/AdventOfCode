module Year2015.Day09
    ( part1
    , part2
    ) where

import Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as M
import Data.List (foldl', permutations)
import Data.Maybe
import Text.Megaparsec
import Text.Megaparsec.Lexer
import Text.Megaparsec.String

data Edge = Edge String String Int

opposite :: Edge -> Edge
opposite (Edge p1 p2 d) = Edge p2 p1 d

parseLine :: String -> Edge
parseLine = fromJust . parseMaybe parser
    where parser :: Parser Edge
          parser = Edge <$> some alphaNumChar <* string " to "
                        <*> some alphaNumChar <* string " = "
                        <*> (fromInteger <$> integer)

allPathDistances :: [String] -> [Int]
allPathDistances input = let m = constructMap $ map parseLine input
                             paths = permutations $ M.keys m
                         in map (\p -> sum . zipWith (\a b -> m ! a ! b) p $ tail p) paths
    where constructMap = foldl' (\m e -> addEdgeToMap (opposite e) $ addEdgeToMap e m) M.empty
          addEdgeToMap (Edge p1 p2 d) m = let m' = fromMaybe M.empty $ M.lookup p1 m
                                          in M.insert p1 (M.insert p2 d m') m

part1 :: String -> String
part1 = show . minimum . allPathDistances . lines

part2 :: String -> String
part2 = show . maximum . allPathDistances . lines
