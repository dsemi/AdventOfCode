module Year2015.Day16
    ( part1
    , part2
    ) where

import Control.Monad
import Data.List (findIndex)
import Data.Maybe
import Text.Megaparsec
import Text.Megaparsec.Char (digitChar, lowerChar, string)
import Text.Megaparsec.Char.Lexer (decimal)


tape :: [(String, (Int -> Bool))]
tape = [ ("children", (==3))
       , ("cats", (==7))
       , ("samoyeds", (==2))
       , ("pomeranians", (==3))
       , ("akitas", (==0))
       , ("vizslas", (==0))
       , ("goldfish", (==5))
       , ("trees", (==3))
       , ("cars", (==2))
       , ("perfumes", (==1))
       ]

parseLines :: String -> [[(String, Int)]]
parseLines = map (fromJust . parseMaybe parser) . lines
    where int = fromInteger <$> decimal
          parser :: Parsec () String [(String, Int)]
          parser = do
            void $ string "Sue " >> some digitChar >> string ": "
            let counts = (,) <$> some lowerChar <* string ": " <*> int
            counts `sepBy1` string ", "

-- Sue 480: goldfish: 1, children: 9, vizslas: 3
solve :: [(String, (Int -> Bool))] -> String -> Maybe Int
solve tape' = fmap succ . findIndex couldMatch . parseLines
    where couldMatch = all (uncurry (fromJust . (`lookup` tape')))

part1 :: String -> Maybe Int
part1 = solve tape

part2 :: String -> Maybe Int
part2 = solve $ [ ("cats", (>7))
                , ("pomeranians", (<3))
                , ("goldfish", (<5))
                , ("trees", (>3))
                ] ++ tape
