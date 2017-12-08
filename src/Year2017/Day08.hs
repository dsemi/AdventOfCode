module Year2017.Day08
    ( part1
    , part2
    ) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M


data Instr = Instr { cmd :: HashMap String Int -> HashMap String Int
                   , cond :: HashMap String Int -> Bool
                   }

parseInstrs :: String -> [Instr]
parseInstrs = map parseInstr . lines
    where parseInstr :: String -> Instr
          parseInstr line =
              let [reg, fn, amnt, "if", reg2, cmp, val] = words line
                  cmd m = let f = case fn of
                                    "inc" -> (+ read amnt)
                                    "dec" -> subtract $ read amnt
                          in M.insert reg (f $ M.lookupDefault 0 reg m) m
                  cond m = let comp = case cmp of
                                         "!=" -> (/=)
                                         "==" -> (==)
                                         ">=" -> (>=)
                                         ">" -> (>)
                                         "<=" -> (<=)
                                         "<" -> (<)
                           in comp (M.lookupDefault 0 reg2 m) $ read val
              in Instr cmd cond

maximumOr :: (Ord a) => a -> [a] -> a
maximumOr x [] = x
maximumOr _ xs = maximum xs

eval :: (HashMap String Int, Int) -> Instr -> (HashMap String Int, Int)
eval (m, max') (Instr cmd cond) =
    let m' = if cond m then cmd m else m
        max'' = max max' $ maximumOr 0 $ M.elems m'
    in (m', max'')

part1 :: String -> Int
part1 = maximumOr 0 . M.elems . fst . foldl eval (M.empty, 0) . parseInstrs

part2 :: String -> Int
part2 = snd . foldl eval (M.empty, 0) . parseInstrs
