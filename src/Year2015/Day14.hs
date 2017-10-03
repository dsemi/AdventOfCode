module Year2015.Day14
    ( part1
    , part2
    ) where

import Utils

import Data.Ord
import Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as M
import qualified Data.IntMultiSet as MS
import Data.List (sortBy, transpose)
import Data.Maybe
import Text.Megaparsec (parseMaybe, some)
import Text.Megaparsec.Char (alphaNumChar, string)
import Text.Megaparsec.Char.Lexer (decimal)


totalTime :: Int
totalTime = 2503

getDistancesAtEachSecond :: String -> HashMap String [Int]
getDistancesAtEachSecond input = M.fromList [ (name, take totalTime distStages)
                                            | (name, speed, flyTime, restTime) <-
                                                map (fromJust . parseMaybe parser) $ lines input
                                            , let distStages = scanl1 (+) . cycle
                                                               $ replicate flyTime speed
                                                               ++ replicate restTime 0
                                            ]
    where int = fromInteger <$> decimal
          parser :: Parser (String, Int, Int, Int)
          parser = do
            name <- some alphaNumChar
            string " can fly "
            speed <- int
            string " km/s for "
            flyTime <- int
            string " seconds, but then must rest for "
            restTime <- int
            string " seconds."
            return (name, speed, flyTime, restTime)

maxesBy :: Ord b => (a -> b) -> [a] -> [a]
maxesBy cmp xs = let ms = sortBy (flip $ comparing cmp) xs
                 in takeWhile ((== cmp (head ms)) . cmp) ms

part1 :: String -> Int
part1 = maximum . map last . M.elems . getDistancesAtEachSecond

part2 :: String -> Int
part2 input = let dists = getDistancesAtEachSecond input
                  counts = MS.fromList . concatMap (map fst . maxesBy snd . zip [1..])
                           . transpose $ M.elems dists
              in snd . last $ MS.toAscOccurList counts
