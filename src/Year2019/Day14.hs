{-# LANGUAGE FlexibleContexts, NumDecimals, TemplateHaskell #-}

module Year2019.Day14
    ( part1
    , part2
    ) where

import Control.Lens
import Control.Monad.State
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer


data ReactState = ReactState { _surplus :: Map String Int
                             , _ore :: Int
                             }
makeLenses ''ReactState

parseReactions :: String -> [(String, (Int, [(Int, String)]))]
parseReactions = map (fromJust . parseMaybe reaction) . lines
    where chemical :: Parsec () String (Int, String)
          chemical = (,) <$> decimal <* spaceChar <*> some upperChar
          reaction :: Parsec () String (String, (Int, [(Int, String)]))
          reaction = do
            ins <- chemical `sepBy` string ", "
            (n, out) <- string " => " *> chemical
            pure (out, (n, ins))

numOre :: [(String, (Int, [(Int, String)]))] -> Int -> Int
numOre m = view ore . flip execState (ReactState M.empty 0) . go "FUEL"
    where go k c = case lookup k m of
                     Just (n, chems) -> do
                       let (q, r) = c `quotRem` n
                       forM_ chems $ \(a, chem) -> do
                         let amt = a * (if r /= 0 then q + 1 else q)
                         val <- M.findWithDefault 0 chem <$> use surplus
                         surplus %= M.insert chem (max 0 (val - amt))
                         when (amt > val) $ go chem (amt - val)
                       when (r /= 0) $ surplus %= M.insertWith (+) k (n - r)
                     Nothing -> ore += c

part1 :: String -> Int
part1 = flip numOre 1 . parseReactions

search :: Int -> Int -> (Int -> Int) -> Int
search a b f
    | a == b = if f a > 1e12 then a - 1 else a
    | f mid > 1e12 = search a (mid - 1) f
    | otherwise = search (mid + 1) b f
    where mid = (a + b) `div` 2

part2 :: String -> Int
part2 = search 0 1e12 . numOre . parseReactions
