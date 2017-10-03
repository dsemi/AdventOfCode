module Year2015.Day19
    ( part1
    , part2
    ) where

import Utils

import Control.Arrow
import Data.ByteString.Char8 (pack)
import Data.HashSet (HashSet)
import qualified Data.HashSet as S
import Data.List (intercalate)
import Data.Maybe
import Data.String.Utils
import Data.Tuple
import Text.Megaparsec (parseMaybe, some)
import Text.Megaparsec.Char (alphaNumChar, string)
import Text.Regex.PCRE.Heavy (Regex, compileM, sub)


parseMapping :: String -> (String, String)
parseMapping = fromJust . parseMaybe parser
    where parser :: Parser (String, String)
          parser = (,) <$> some alphaNumChar <* string " => " <*> some alphaNumChar

-- E.g. singleReplacements "aa" "xx" "abskaalkjdsaajlkdaa" ->
--   ["abskxxlkjdsaajlkdaa", "abskaalkjdsxxjlkdaa", "abskaalkjdsaajlkdxx"]
singleReplacements :: String -> String -> String -> [String]
singleReplacements k v src = map (intercalate k) parts
    where pieces = split k src
          parts = [ snd $ foldr (\p (i, s) ->
                                     ( i-1
                                     , if i == 0
                                       then (p ++ v ++ head s) : tail s
                                       else p : s
                                     )) (i, []) pieces
                  | i <- [ 1 .. length pieces - 1 ]
                  ]

uniqueSubs :: [(String, String)] -> String -> HashSet String
uniqueSubs reps src = S.fromList $ concat [ singleReplacements k v src | (k, v) <- reps]

findPathToElectron :: Regex -> (String -> String) -> String -> Int
findPathToElectron regex rep = go 0
    where go c "e" = c
          go c s = go (c+1) $ sub regex rep s

part1 :: String -> Int
part1 input = let (s:_:mappings) = reverse $ lines input
                  reps = map parseMapping mappings
              in S.size $ uniqueSubs reps s

part2 :: String -> Int
part2 input = let (s:_:mappings) = reverse $ lines input
                  s' = reverse s
                  reps = map (swap . (reverse *** reverse) . parseMapping) mappings
                  (Right regex) = compileM (pack . intercalate "|" $ map fst reps) []
                  rep w = fromJust $ lookup w reps
              in findPathToElectron regex rep s'
