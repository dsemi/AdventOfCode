{-# LANGUAGE OverloadedStrings #-}

module Year2020.Day07
    ( part1
    , part2
    ) where

import Data.Map (Map, (!))
import qualified Data.Map as M
import Data.Maybe
import Text.Megaparsec
import Text.Megaparsec.Char.Lexer


parseBags :: String -> Map String [(Int, String)]
parseBags = M.fromList . map (fromJust . parseMaybe @() parser) . lines
    where bag = manyTill anySingle $ " bag" >> optional "s" >> optional "."
          parser = do
            key <- manyTill anySingle " bags contain "
            vals <- ("no other bags." *> pure [])
                    <|> ((,) <$> decimal <* " " <*> bag) `sepBy` ", "
            pure (key, vals)

part1 :: String -> Int
part1 (parseBags -> bags) = length $ filter holdsShinyGold $ M.keys bags
    where holdsShinyGold = any (\(_, k) -> k == "shiny gold" || holdsShinyGold k) . (bags !)

part2 :: String -> Int
part2 (parseBags -> bags) = countBags "shiny gold"
    where countBags = sum . map (\(n, k) -> n + n * countBags k) . (bags !)
