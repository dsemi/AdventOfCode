module Year2015.Day09
    ( part1
    , part2
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.HashMap.Strict ((!))
import qualified Data.HashMap.Strict as M
import Data.List (permutations)
import FlatParse.Basic


data Edge = Edge ByteString ByteString Int

opposite :: Edge -> Edge
opposite (Edge p1 p2 d) = Edge p2 p1 d

parseLine :: ByteString -> Edge
parseLine line = case runParser parser line of
                   OK edge _ -> edge
                   _ -> error "unreachable"
    where letter = byteStringOf (some $ satisfy (\x -> isDigit x || isLatinLetter x))
          parser = Edge <$> letter <* $(string " to ")
                        <*> letter <* $(string " = ")
                        <*> anyAsciiDecimalInt

allPathDistances :: ByteString -> [Int]
allPathDistances input = let m = constructMap $ map parseLine $ B.lines input
                             paths = permutations $ M.keys m
                         in map (\p -> sum . zipWith (\a b -> m ! a ! b) p $ tail p) paths
    where constructMap = foldr (\e -> addEdgeToMap (opposite e) . addEdgeToMap e) M.empty
          addEdgeToMap (Edge p1 p2 d) m = let m' = M.lookupDefault M.empty p1 m
                                          in M.insert p1 (M.insert p2 d m') m

part1 :: ByteString -> Int
part1 = minimum . allPathDistances

part2 :: ByteString -> Int
part2 = maximum . allPathDistances
