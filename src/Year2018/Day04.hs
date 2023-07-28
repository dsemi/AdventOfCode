module Year2018.Day04
    ( part1
    , part2
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Char
import Data.List (maximumBy, sort)
import Data.IntMap.Strict (IntMap, (!))
import qualified Data.IntMap.Strict as M
import Data.Ord
import FlatParse.Basic

data Record = GuardChange { guardNum :: Int }
            | SleepToggle { minute :: Int }

parseRecords :: ByteString -> [Record]
parseRecords = map parse . sort . B.lines
    where int = anyAsciiDecimalInt
          parser = do
            m <- $(char '[') *> int *> $(char '-') *> int *> $(char '-') *> int *>
                 skipSatisfy isSpace *> int *> $(char ':') *> int <* $(char ']')
            maybe (SleepToggle m) GuardChange <$> (skipSatisfy isSpace >> optional ($(string "Guard #") *> int))
          parse line = case runParser parser line of
                         OK res _ -> res
                         _ -> error "unreachable"

guardSleepFreqs :: [Record] -> IntMap [Int]
guardSleepFreqs = M.fromListWith (zipWith (+)) . projectShifts
    where isShiftChange (GuardChange _) = True
          isShiftChange _ = False
          projectShifts [] = []
          projectShifts (r:rs) = (guardNum r, hourStates shift) : projectShifts rest
              where (shift, rest) = break isShiftChange rs
          hourStates rs = concat $ zipWith replicate (zipWith subtract xs $ tail xs) $ cycle [0, 1]
              where xs = 0 : map minute rs ++ [60]

part1 :: ByteString -> Int
part1 input = let sleepFreqs = guardSleepFreqs $ parseRecords input
                  n = fst $ maximumBy (comparing snd) $ M.toList $ M.map sum sleepFreqs
                  m = fst $ maximumBy (comparing snd) $ zip [0..] $ sleepFreqs ! n
              in n * m

part2 :: ByteString -> Int
part2 input = let sleepFreqs = guardSleepFreqs $ parseRecords input
                  (n, (m, _)) = maximumBy (comparing (snd . snd)) $ M.toList
                                $ M.map (maximumBy (comparing snd) . zip [0..]) sleepFreqs
              in n * m
