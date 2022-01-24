module Year2021.Day10
    ( part1
    , part2
    ) where

import Data.Either
import Data.List (elemIndex, foldl1', sort)
import Data.Maybe
import Text.Megaparsec

data Open  = OParen | OBracket | OBrace | OCaret deriving (Enum)
data Close = CParen | CBracket | CBrace | CCaret deriving (Enum)

analyze :: String -> Either Int Int
analyze = go [] . stream
    where stream :: String -> [Either Open Close]
          stream = fromJust . parseMaybe @() (some (eitherP open close))
              where open  = toEnum . fromJust . (`elemIndex` "([{<") <$> oneOf "([{<"
                    close = toEnum . fromJust . (`elemIndex` ")]}>") <$> oneOf ")]}>"
          go ss (Left x:xs) = go (x:ss) xs
          go (s:ss) (Right x:xs)
              | fromEnum s == fromEnum x = go ss xs
              | otherwise = Left $ case x of
                                     CParen -> 3
                                     CBracket -> 57
                                     CBrace -> 1197
                                     CCaret -> 25137
          go ss [] = Right $ foldl1' (\a b -> a * 5 + b) $ map ((+1) . fromEnum) ss

part1 :: String -> Int
part1 = sum . lefts . map analyze . lines

part2 :: String -> Int
part2 input = ns !! (length ns `div` 2)
    where ns = sort $ rights $ map analyze $ lines input
