module Year2021.Day10
    ( part1
    , part2
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Either
import Data.List (elemIndex, foldl1', sort)
import Data.Maybe
import FlatParse.Basic

data Open  = OParen | OBracket | OBrace | OCaret deriving (Enum)
data Close = CParen | CBracket | CBrace | CCaret deriving (Enum)

analyze :: ByteString -> [Either Int Int]
analyze = map (go [] . stream) . B.lines
    where stream :: ByteString -> [Either Open Close]
          stream line = case runParser (some $ open <|> close) line of
                          OK res _ -> res
                          _ -> error "unreachable"
              where open  = Left . toEnum . fromJust . (`elemIndex` "([{<") <$> satisfy (`elem` "([{<")
                    close = Right . toEnum . fromJust . (`elemIndex` ")]}>") <$> satisfy (`elem` ")]}>")
          go ss (Left x:xs) = go (x:ss) xs
          go (s:ss) (Right x:xs)
              | fromEnum s == fromEnum x = go ss xs
              | otherwise = Left $ case x of
                                     CParen -> 3
                                     CBracket -> 57
                                     CBrace -> 1197
                                     CCaret -> 25137
          go ss [] = Right $ foldl1' (\a b -> a * 5 + b) $ map ((+1) . fromEnum) ss

part1 :: ByteString -> Int
part1 = sum . lefts . analyze

part2 :: ByteString -> Int
part2 input = ns !! (length ns `div` 2)
    where ns = sort $ rights $ analyze input
