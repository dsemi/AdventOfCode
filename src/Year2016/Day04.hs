{-# LANGUAGE NamedFieldPuns #-}

module Year2016.Day04
    ( part1
    , part2
    ) where

import Control.Arrow ((&&&))
import Data.List (group, intercalate, isInfixOf, sort)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import Text.Megaparsec
import Text.Megaparsec.Char (char, noneOf)


data Room = Room { encryptedName :: String
                 , sectorId :: Int
                 , checksum :: String
                 }

getRooms :: String -> [Room]
getRooms = mapMaybe (parseMaybe parseRoom) . lines
    where parseRoom :: Parsec () String Room
          parseRoom = do
            encryptedPlusSector <- some (noneOf "[") <* char '['
            checksum' <- some (noneOf "]") <* char ']'
            let ss = splitOn "-" encryptedPlusSector
            return $ Room (intercalate "-" $ init ss) (read $ last ss) checksum'

roomIsValid :: Room -> Bool
roomIsValid (Room {encryptedName, checksum}) =
    nCommonChars 5 (filter (/= '-') encryptedName) == checksum
    where nCommonChars n = take n . map snd . sort . map (negate . length &&& head) . group . sort

part1 :: String -> Int
part1 = sum . map sectorId . filter roomIsValid . getRooms

rotate :: Int -> Char -> Char
rotate 0 c = c
rotate n c
    | c == '-' = ' '
    | c == 'z' = rotate (n-1) 'a'
    | otherwise= rotate (n-1) (succ c)

isNorthPole :: Room -> Bool
isNorthPole (Room en si _) = "northpole" `isInfixOf` map (rotate si) en

part2 :: String -> Int
part2 = sectorId . head . filter isNorthPole . filter roomIsValid . getRooms
