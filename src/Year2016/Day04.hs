module Year2016.Day04
    ( part1
    , part2
    ) where

import Control.Arrow ((&&&))
import Data.List (group, intercalate, isInfixOf, sort, sortBy)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Text.Megaparsec (char, noneOf, parseMaybe, some)
import Text.Megaparsec.String (Parser)


data Room = Room { encryptedName :: String
                 , sectorId :: Int
                 , checksum :: String
                 }

getRooms :: String -> [Room]
getRooms = map (fromJust . parseMaybe parseRoom) . lines
    where parseRoom :: Parser Room
          parseRoom = do
            encryptedPlusSector <- some (noneOf "[") <* char '['
            checksum <- some (noneOf "]") <* char ']'
            let ss = splitOn "-" encryptedPlusSector
            return $ Room (intercalate "-" $ init ss) (read $ last ss) checksum

roomIsValid :: Room -> Bool
roomIsValid (Room en _ ch) = nCommonChars 5 (filter (/= '-') en) == ch
    where nCommonChars n = take n . map snd . sort . map (negate . length &&& head) . group . sort

part1 :: String -> String
part1 = show . sum . map sectorId . filter roomIsValid . getRooms

rotate 0 c = c
rotate n c
    | c == '-' = ' '
    | c == 'z' = rotate (n-1) 'a'
    | otherwise= rotate (n-1) (succ c)

isNorthPole :: Room -> Bool
isNorthPole (Room en si _) = "northpole" `isInfixOf` map (rotate si) en

part2 :: String -> String
part2 = show . sectorId . head . filter isNorthPole . filter roomIsValid . getRooms
