module Year2015.Day13
    ( part1
    , part2
    ) where

import Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as M
import Data.List (foldl', permutations)
import Data.Maybe
import Text.Megaparsec
import Text.Megaparsec.Lexer
import Text.Megaparsec.String

data Edge = Edge String String Int

parseLine :: String -> Edge
parseLine = fromJust . parseMaybe parser
    where int = fromInteger <$> integer
          parseValue :: Parser Int
          parseValue = do
            op <- (try (string "lose") <|> (string "gain")) <* spaceChar
            i <- int
            let op' = if op == "lose" then negate else id
            return $ op' i
          parser :: Parser Edge
          parser = do
            p1 <- some alphaNumChar
            string " would "
            hap <- parseValue
            string " happiness units by sitting next to "
            p2 <- some alphaNumChar
            char '.'
            return $ Edge p1 p2 hap



constructMap :: [Edge] -> HashMap String (HashMap String Int)
constructMap = foldl' addEdgeToMap M.empty
    where addEdgeToMap m (Edge p1 p2 n) = let m' = fromMaybe M.empty $ M.lookup p1 m
                                          in M.insert p1 (M.insert p2 n m') m

maxHappinessOrdering :: HashMap String (HashMap String Int) -> Int
maxHappinessOrdering m = maximum $ map (\p -> happinessDiff (head p) (last p)
                                              + sum (zipWith happinessDiff p $ tail p)) orders
    where orders = permutations $ M.keys m
          happinessDiff a b = m ! a ! b + m ! b ! a

part1 :: String -> String
part1 = show . maxHappinessOrdering . constructMap . map parseLine . lines

p2 :: String -> Int
p2 input = let m = constructMap . map parseLine $ lines input
               meMap = M.fromList . zip (M.keys m) $ repeat 0
               m' = M.insert "me" meMap $ M.map (M.insert "me" 0) m
           in maxHappinessOrdering m'

part2 :: String -> String
part2 = show . p2
