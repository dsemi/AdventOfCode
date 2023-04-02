module Year2017.Day12
    ( part1
    , part2
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.List (foldl')
import Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import FlatParse.Basic

import Utils

parse :: ByteString -> HashMap Int [Int]
parse input = M.fromList [ case runParser parser line of
                             OK res _ -> res
                             _ -> error "unreachable"
                         | line <- B.lines input ]
    where num = anyAsciiDecimalInt
          parser = (,) <$> num <* $(string " <-> ") <*> some (num <* optional_ $(string ", "))

part1 :: ByteString -> Int
part1 x = length $ bfs 0 (parse x !)

part2 :: ByteString -> Int
part2 x = fst $ foldl' f (0, S.empty) $ M.keys m
    where m = parse x
          f (c, set) n
              | S.member n set = (c, set)
              | otherwise = (c+1, S.union set $ S.fromList $ map snd $ bfs n (m !))
