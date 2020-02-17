module Year2015.Day11
    ( part1
    , part2
    ) where

import Utils

import Data.Either
import Data.List (tails)
import Text.Megaparsec
import Text.Megaparsec.Char (char)


incrStr :: String -> String
incrStr = reverse . step . reverse
    where step [] = []
          step (x:xs)
              | x == 'z' = 'a' : step xs
              | otherwise = succ x : xs

isValid :: String -> Bool
isValid s = not (any (`elem` s) "iol") && isSuccessive s
            && length (fromRight undefined $ parse parser "" s) > 1
    where dupChars = do
            a <- anySingle >>= char
            return $ a : a : ""
          parser :: Parsec () String [String]
          parser = try (many (searchAll dupChars)) <|> return []
          isSuccessive = any ordered . windows 3
              where ordered []  = True
                    ordered [_] = True
                    ordered (x:y:xs)
                        | y == succ x = ordered $ y : xs
                        | otherwise   = False
                    windows n = takeWhile ((==n) . length)
                                . map (take n) . tails

part1 :: String -> String
part1 = head . filter isValid . tail . iterate incrStr

part2 :: String -> String
part2 = (!! 1) . filter isValid . tail . iterate incrStr
