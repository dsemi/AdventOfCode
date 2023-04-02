{-# LANGUAGE OverloadedStrings #-}

module Year2020.Day21
    ( part1
    , part2
    ) where

import Control.Lens
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import Data.List (partition, sortBy)
import Data.Ord
import Data.Set (Set, (\\))
import qualified Data.Set as S
import FlatParse.Basic hiding (isolate)

parseFoods :: ByteString -> [([ByteString], [ByteString])]
parseFoods = map parse . B.lines
    where word = byteStringOf $ some $ satisfy isLatinLetter
          pFood = (,) <$> (some $ word <* $(char ' '))
                  <*> ($(string "(contains ") *> (some $ word <* optional_ $(string ", ")) <* $(char ')'))
          parse line = case runParser pFood line of
                         OK res _ -> res
                         _ -> error "unreachable"

allergens :: [([ByteString], [ByteString])] -> HashMap ByteString (Set ByteString)
allergens = M.fromListWith S.intersection . concatMap process
    where process (ings, alls) = map (,S.fromList ings) alls

countSafeIngredients :: [([ByteString], [ByteString])] -> Int
countSafeIngredients foods = length $ filter (`S.member` safeIngredients) ingredients
    where ingredients = concatMap fst foods
          safeIngredients = foldr (flip (\\)) (S.fromList ingredients) $ M.elems $ allergens foods

part1 :: ByteString -> Int
part1 = countSafeIngredients . parseFoods

isolate :: HashMap ByteString (Set ByteString) -> [(ByteString, ByteString)]
isolate = map (_2 %~ S.findMin) . until (all ((==1) . S.size . snd)) go . M.toList
    where go m = let (done, left) = partition ((==1) . S.size . snd) m
                     ings = S.unions $ map snd done
                 in done ++ map (_2 %~ (\\ ings)) left

part2 :: ByteString -> ByteString
part2 = B.intercalate "," . map snd . sortBy (comparing fst) . isolate . allergens . parseFoods
