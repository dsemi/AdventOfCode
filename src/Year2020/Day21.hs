{-# LANGUAGE OverloadedStrings #-}

module Year2020.Day21
    ( part1
    , part2
    ) where

import Control.Lens
import Data.List (intercalate, partition, sortBy)
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set, (\\))
import qualified Data.Set as S
import Data.Ord
import Text.Megaparsec
import Text.Megaparsec.Char


parseFoods :: String -> [([String], [String])]
parseFoods = fromJust . parseMaybe @() (pFood `sepBy` "\n")
    where pFood = (,) <$> (some letterChar `endBy` " ")
                  <*> ("(contains " *> (some letterChar `sepBy` ", ") <* ")")

allergens :: [([String], [String])] -> Map String (Set String)
allergens = foldr process M.empty
    where process (ings, alls) m = foldr (\x -> M.insertWith S.intersection x ingSet) m alls
              where ingSet = S.fromList ings

countSafeIngredients :: [([String], [String])] -> Int
countSafeIngredients foods = length $ filter (`S.member` safeIngredients) ingredients
    where ingredients = concatMap fst foods
          safeIngredients = foldr (flip (\\)) (S.fromList ingredients) $ M.elems $ allergens foods

part1 :: String -> Int
part1 = countSafeIngredients . parseFoods

isolate :: Map String (Set String) -> [(String, String)]
isolate = map (_2 %~ S.findMin) . until (all ((==1) . S.size . snd)) go . M.toList
    where go m = let (done, left) = partition ((==1) . S.size . snd) m
                     ings = S.unions $ map snd done
                 in done ++ map (_2 %~ (\\ ings)) left

part2 :: String -> String
part2 = intercalate "," . map snd . sortBy (comparing fst) . isolate . allergens . parseFoods
