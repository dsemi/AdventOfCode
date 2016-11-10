module Year2015.Day14
    ( part1
    , part2
    ) where

import Data.Ord
import Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as M
import qualified Data.IntMultiSet as MS
import Data.List (sortBy, transpose)
import Data.Maybe
import Text.Megaparsec
import Text.Megaparsec.Lexer
import Text.Megaparsec.String

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
    where int = fromInteger <$> integer
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

part1 :: String -> String
part1 = show . maximum . map last . M.elems . getDistancesAtEachSecond

p2 :: String -> Int
p2 input = let dists = getDistancesAtEachSecond input
               counts = MS.fromList . concatMap (map fst . maxesBy snd . zip [1..])
                        . transpose $ M.elems dists
           in snd . last $ MS.toAscOccurList counts

part2 :: String -> String
part2 = show . p2