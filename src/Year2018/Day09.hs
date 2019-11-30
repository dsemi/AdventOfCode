module Year2018.Day09
    ( part1
    , part2
    ) where

import Control.Lens
import Data.List.PointedList.Circular
import qualified Data.IntMap.Strict as M
import Text.Megaparsec (Parsec, parseMaybe)
import Text.Megaparsec.Char (string)
import Text.Megaparsec.Char.Lexer (decimal)


parse :: String -> Maybe (Int, Int)
parse = parseMaybe parser
    where parser :: Parsec () String (Int, Int)
          parser = do
            a <- decimal <* string " players; last marble is worth "
            b <- decimal <* string " points"
            pure (a, b)

play :: (Int, Int) -> Maybe Int
play (n, s) = go 1 M.empty (singleton 0)
    where go p m c
              | p == s+1 = Just $ maximum $ M.elems m
              | p `rem` 23 /= 0 = go (p+1) m $ insertRight p (next c)
              | otherwise = let c' = moveN (-7) c
                                m' = M.insertWith (+) (p `rem` n) (p + c' ^. focus) m
                            in delete c' >>= go (p+1) m'

part1 :: String -> Maybe Int
part1 x = parse x >>= play

part2 :: String -> Maybe Int
part2 x = parse x >>= play . (_2 *~ 100)
