module Year2016.Day04
    ( part1
    , part2
    ) where

import Control.Arrow ((&&&))
import Control.Monad
import Data.Char
import Data.List (group, intercalate, isInfixOf, sort)
import Data.Maybe (mapMaybe)
import Text.Megaparsec
import Text.Megaparsec.Char (char, lowerChar)
import Text.Megaparsec.Char.Lexer (decimal)


data Room = Room { name :: String
                 , sid :: Int
                 }

getRooms :: String -> [Room]
getRooms = mapMaybe (parseMaybe parseRoom) . lines
    where parseRoom :: Parsec () String Room
          parseRoom = do
            room <- Room <$> (intercalate "-" <$> some lowerChar `endBy` char '-') <*> decimal
            checksum <- between (char '[') (char ']') (some lowerChar)
            guard $ (== checksum) $ take 5 $ map snd $ sort
                      $ map (negate . length &&& head) $ group
                      $ sort $ filter (/= '-') $ name room
            pure room

part1 :: String -> Int
part1 = sum . map sid . getRooms

rotate :: Int -> Char -> Char
rotate _ '-' = ' '
rotate n c = chr $ (ord c - n - 97) `mod` 26 + 97

isNorthPole :: Room -> Bool
isNorthPole (Room en si) = map (rotate si) "northpole" `isInfixOf` en

part2 :: String -> Int
part2 = sid . head . filter isNorthPole . getRooms
