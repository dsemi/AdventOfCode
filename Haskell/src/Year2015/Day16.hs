module Year2015.Day16
    ( part1
    , part2
    ) where

import Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as M
import Data.Maybe
import Text.Megaparsec
import Text.Megaparsec.Lexer
import Text.Megaparsec.String

tape :: HashMap String (Int -> Bool)
tape = M.fromList [ ("children", (==3))
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

parseLines :: String -> [HashMap String Int]
parseLines = map (M.fromList . fromJust . parseMaybe parser) . lines
    where int = fromInteger <$> integer
          parser :: Parser [(String, Int)]
          parser = do
            string "Sue " >> some digitChar >> string ": "
            let counts = (,) <$> some lowerChar <* string ": " <*> int
            counts `sepBy1` string ", "

-- Sue 480: goldfish: 1, children: 9, vizslas: 3
couldMatch :: HashMap String (Int -> Bool) -> HashMap String Int -> Bool
couldMatch tape = all (uncurry (tape !)) . M.toList

solve :: HashMap String (Int -> Bool) -> String -> String
solve tape' = show . fst . head . filter (couldMatch tape' . snd) . zip [1..] . parseLines

part1 :: String -> String
part1 = solve tape

part2 :: String -> String
part2 = solve tape'
    where tape' = M.fromList [ ("cats", (>7))
                             , ("pomeranians", (<3))
                             , ("goldfish", (<5))
                             , ("trees", (>3))
                             ] `M.union` tape