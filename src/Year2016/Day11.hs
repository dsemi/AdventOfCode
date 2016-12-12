module Year2016.Day11
    ( part1
    , part2
    ) where

import Utils (findAll)

import Control.Lens ((%~), ix)
import Data.List (inits)
import Text.Megaparsec (choice, noneOf, some, spaceChar, string)


parseFloor :: String -> [String]
parseFloor = findAll $ do
               name <- spaceChar *> some (noneOf " ")
               type' <- spaceChar *> choice (map string ["microchip", "generator"])
               return $ unwords [name, type']

countSteps :: [[String]] -> Int
countSteps = sum . map (subtract 3 . (*2) . sum) . tail . inits . init . map length

part1 :: String -> Int
part1 = countSteps . map parseFloor . lines

part2 :: String -> Int
part2 = countSteps . (ix 0 %~ (++ extraItems)) . map parseFloor . lines
    where extraItems = [ "elerium generator"
                       , "elerium-compatible microchip"
                       , "dilithium generator"
                       , "dilithium-compatible microchip"
                       ]
