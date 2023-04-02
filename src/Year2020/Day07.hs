module Year2020.Day07
    ( part1
    , part2
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import FlatParse.Basic

import Utils

parseBags :: ByteString -> HashMap String [(Int, String)]
parseBags = M.fromList . map parse . B.lines
    where bag = manyTill anyAsciiChar $
                $(string " bag") >> optional_ $(char 's') >> optional_ $(char '.')
          parser = do
            key <- manyTill anyAsciiChar $(string " bags contain ")
            vals <- ($(string "no other bags.") *> pure [])
                    <|> some ((,) <$> anyAsciiDecimalInt <* $(char ' ') <*> bag <* optional_ $(string ", "))
            pure (key, vals)
          parse line = case runParser parser line of
                         OK res _ -> res
                         _ -> error "unreachable"

part1 :: ByteString -> Int
part1 (parseBags -> bags) = S.size $ S.fromList $ go "shiny gold"
    where rev = M.fromListWith (++) . concatMap (\(k, v) -> map ((,[k]) . snd) v) $ M.toList bags
          go k = let v = M.findWithDefault [] k rev in v ++ concatMap go v

part2 :: ByteString -> Int
part2 (parseBags -> bags) = countBags "shiny gold"
    where countBags = sum . map (\(n, k) -> n + n * countBags k) . (bags !)
