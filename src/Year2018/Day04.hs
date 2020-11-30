{-# LANGUAGE QuasiQuotes #-}

module Year2018.Day04
    ( part1
    , part2
    ) where

import Data.Either
import Data.List (maximumBy, sort)
import Data.IntMap.Strict (IntMap, (!))
import qualified Data.IntMap.Strict as M
import Data.Ord
import Text.Megaparsec (Parsec, optional, parse)
import Text.Megaparsec.Char (char, space, string)
import Text.Megaparsec.Char.Lexer (decimal)


data Record = GuardChange { guardNum :: Int }
            | SleepToggle { minute :: Int }

parseRecords :: String -> [Record]
parseRecords = rights . map (parse parser "") . sort . lines
    where parser :: Parsec () String Record
          parser = do
            m <- char '[' *> decimal *> char '-' *> decimal *> char '-' *> decimal *>
                 space *> decimal *> char ':' *> decimal <* char ']'
            maybe (SleepToggle m) GuardChange <$> (space >> optional (string "Guard #" *> decimal))

guardSleepFreqs :: [Record] -> IntMap [Int]
guardSleepFreqs = M.fromListWith (zipWith (+)) . projectShifts
    where isShiftChange (GuardChange _) = True
          isShiftChange _ = False
          projectShifts [] = []
          projectShifts (r:rs) = (guardNum r, hourStates shift) : projectShifts rest
              where (shift, rest) = break isShiftChange rs
          hourStates rs = concat $ zipWith replicate (zipWith subtract xs $ tail xs) $ cycle [0, 1]
              where xs = 0 : map minute rs ++ [60]

part1 :: String -> Int
part1 input = let sleepFreqs = guardSleepFreqs $ parseRecords input
                  n = fst $ maximumBy (comparing snd) $ M.toList $ M.map sum sleepFreqs
                  m = fst $ maximumBy (comparing snd) $ zip [0..] $ sleepFreqs ! n
              in n * m

part2 :: String -> Int
part2 input = let sleepFreqs = guardSleepFreqs $ parseRecords input
                  (n, (m, _)) = maximumBy (comparing (snd . snd)) $ M.toList
                                $ M.map (maximumBy (comparing snd) . zip [0..]) sleepFreqs
              in n * m
