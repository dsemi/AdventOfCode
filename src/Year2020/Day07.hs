{-# LANGUAGE OverloadedStrings #-}

module Year2020.Day07
    ( part1
    , part2
    ) where

import Data.Map (Map, (!))
import qualified Data.Map as M
import qualified Data.Set as S
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
part1 (parseBags -> bags) = S.size $ S.fromList $ go "shiny gold"
    where rev = M.fromListWith (++) . concatMap (\(k, v) -> map ((,[k]) . snd) v) $ M.toList bags
          go k = let v = M.findWithDefault [] k rev in v ++ concatMap go v

part2 :: String -> Int
part2 (parseBags -> bags) = countBags "shiny gold"
    where countBags = sum . map (\(n, k) -> n + n * countBags k) . (bags !)
