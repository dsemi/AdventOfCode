module Year2021.Day11
    ( part1
    , part2
    ) where

import Control.Monad.State.Strict
import Data.Char
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import Data.List (elemIndex)

type Grid = HashMap (Int, Int) Int

flash :: (Int, Int) -> State Grid Int
flash (r, c) = do
  modify' $ M.insert (r, c) (-1)
  fmap ((+1) . sum) $ forM ((,) <$> [r-1, r, r+1] <*> [c-1, c, c+1]) $ \rc -> do
    v <- M.findWithDefault (-1) rc <$> get
    when (v /= -1) $ modify' $ M.adjust (+1) rc
    if v >= 9 then flash rc else pure 0

run :: String -> [Int]
run input = go $ M.fromList [ ((r, c), v) | (r, row) <- zip [0..] $ lines input
                            , (c, v) <- zip [0..] $ map digitToInt row]
    where go grid = let grid' = M.map ((+1) . max 0) grid
                        (n, grid'') = flip runState grid' $ do
                                        fmap sum $ forM (M.keys grid') $ \rc -> do
                                            v <- M.findWithDefault 0 rc <$> get
                                            if v > 9 then flash rc else pure 0
                    in n : go grid''

part1 :: String -> Int
part1 = sum . take 100 . run

part2 :: String -> Maybe Int
part2 = fmap (+1) . elemIndex 100 . run
