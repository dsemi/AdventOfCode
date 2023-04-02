module Year2018.Day08
    ( part1
    , part2
    ) where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Char
import Data.List.Extra ((!?))
import Data.Maybe (mapMaybe)
import Data.Tree
import FlatParse.Basic

parseNodes :: ByteString -> Tree [Int]
parseNodes input = case runParser parseNode input of
                     OK res _ -> res
                     _ -> error "unreachable"
    where space = optional_ $ satisfy isSpace
          parseNode = do
            n <- space *> anyAsciiDecimalInt
            m <- space *> anyAsciiDecimalInt
            flip Node <$> replicateM n parseNode <*> replicateM m (space *> anyAsciiDecimalInt)

part1 :: ByteString -> Int
part1 = sum . fmap sum . parseNodes

part2 :: ByteString -> Int
part2 = go . parseNodes
    where go :: Tree [Int] -> Int
          go (Node metadata []) = sum metadata
          go (Node metadata cs) = sum $ map go $ mapMaybe ((cs !?) . pred) metadata
