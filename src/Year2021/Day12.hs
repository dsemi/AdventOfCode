module Year2021.Day12
    ( part1
    , part2
    ) where

import Control.Monad.State.Strict
import Data.Char
import Data.HashMap.Strict ((!), HashMap)
import qualified Data.HashMap.Strict as M
import Data.List.Split (splitOn)

parse :: String -> HashMap String [String]
parse = M.fromListWith (++) . concatMap f . lines
    where f line = let [k, v] = splitOn "-" line
                   in [(k, [v]), (v, [k])]

dfs :: Bool -> HashMap String [String] -> Int
dfs double m = evalState (go "start" double) M.empty
    where go :: String -> Bool -> State (HashMap String Int) Int
          go "end" _ = pure 1
          go k dbl = do
            vis <- get
            let b = all isLower k && M.findWithDefault 0 k vis > 0
            if b && (dbl || k == "start")
            then pure 0
            else do
              modify' $ M.insertWith (+) k 1
              s <- sum <$> mapM (\child -> go child (dbl || b)) (m ! k)
              modify' $ M.adjust (subtract 1) k
              pure s

part1 :: String -> Int
part1 = dfs True . parse

part2 :: String -> Int
part2 = dfs False . parse
