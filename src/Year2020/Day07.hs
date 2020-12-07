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
parseBags = M.fromList . map (fromJust . parseMaybe parser) . lines
    where bag = manyTill anySingle $
                try (chunk " bag" >> optional (single 's') >> optional (single '.'))
          parser :: Parsec () String (String, [(Int, String)])
          parser = do
            key <- manyTill anySingle (try $ chunk " bags contain ")
            vals <- try (chunk "no other bags." *> pure [])
                    <|> (((,) <$> decimal <* single ' ' <*> bag) `sepBy` chunk ", ")
            pure (key, vals)

part1 :: String -> Int
part1 (parseBags -> bags) = length $ filter holdsShinyGold $ M.keys bags
    where holdsShinyGold = any (\(_, k) -> k == "shiny gold" || holdsShinyGold k) . (bags !)

part2 :: String -> Int
part2 (parseBags -> bags) = countBags "shiny gold"
    where countBags = sum . map (\(n, k) -> n + n * countBags k) . (bags !)
