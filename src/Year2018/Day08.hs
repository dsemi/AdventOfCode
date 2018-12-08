module Year2018.Day08
    ( part1
    , part2
    ) where

import Utils

import Data.Maybe (fromJust, fromMaybe)
import Data.Tree
import Text.Megaparsec (count, parseMaybe)
import Text.Megaparsec.Char (space)
import Text.Megaparsec.Char.Lexer (decimal)


parseNodes :: String -> Tree [Int]
parseNodes = fromJust . parseMaybe parse
    where parse :: Parser (Tree [Int])
          parse = do
            n <- space *> decimal
            m <- space *> decimal
            flip Node <$> count n parse <*> count m (space *> decimal)

part1 :: String -> Int
part1 = sum . fmap sum . parseNodes

part2 :: String -> Int
part2 = go . parseNodes
    where go (Node metadata []) = sum metadata
          go (Node metadata children) =
              sum $ map (fromMaybe 0 . fmap go . flip lookup (zip [1..] children)) metadata
