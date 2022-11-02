module Year2018.Day08
    ( part1
    , part2
    ) where

import Data.List.Extra ((!?))
import Data.Maybe (fromJust, mapMaybe)
import Data.Tree
import Text.Megaparsec
import Text.Megaparsec.Char (space)
import Text.Megaparsec.Char.Lexer (decimal)


parseNodes :: String -> Tree [Int]
parseNodes = fromJust . parseMaybe parseNode
    where parseNode :: Parsec () String (Tree [Int])
          parseNode = do
            n <- space *> decimal
            m <- space *> decimal
            flip Node <$> count n parseNode <*> count m (space *> decimal)

part1 :: String -> Int
part1 = sum . fmap sum . parseNodes

part2 :: String -> Int
part2 = go . parseNodes
    where go :: Tree [Int] -> Int
          go (Node metadata []) = sum metadata
          go (Node metadata cs) = sum $ map go $ mapMaybe ((cs !?) . pred) metadata
