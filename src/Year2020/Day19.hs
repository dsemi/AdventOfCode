module Year2020.Day19
    ( part1
    , part2
    ) where

import Data.ByteString (ByteString)
import Data.Char (isLower)
import Data.IntMap (IntMap, (!))
import qualified Data.IntMap as M
import FlatParse.Basic

type Rule = Either Char [[Int]]

parseInput :: ByteString -> (IntMap Rule, [String])
parseInput input = case runParser total input of
                     OK res _ -> res
                     _ -> error "unreachable"
    where word = some $ satisfy isLower
          total = (,) <$> (M.fromList <$> some (rule <* $(char '\n'))) <*> (some $ $(char '\n') >> word)
          rule = (,) <$> anyAsciiDecimalInt <* $(char ':') <*>
                 ((Left <$> ($(string " \"") *> anyAsciiChar <* $(char '"'))) <|>
                  (Right <$> some (some ($(char ' ') *> anyAsciiDecimalInt) <* optional_ $(string " |"))))

solve :: [(Int, Rule)] -> ByteString -> Int
solve extras input = length $ filter (\msg -> check msg [0]) msgs
    where (rules, msgs) = parseInput input
          rules' = M.fromList extras `M.union` rules
          check [] [] = True
          check _  [] = False
          check [] _  = False
          check s (r:ls) = case rules' ! r of
                            Left c -> head s == c && check (tail s) ls
                            Right rss -> any (\rs -> check s $ rs ++ ls) rss

part1 :: ByteString -> Int
part1 = solve []

part2 :: ByteString -> Int
part2 = solve [ (8, Right [[42], [42, 8]])
              , (11, Right [[42, 31], [42, 11, 31]]) ]
