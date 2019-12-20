module Year2019.Day18
    ( part1
    , part2
    ) where

import Control.Lens
import Control.Monad.State
import Data.Array.Unboxed
import Data.Char
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Linear.V2


parseMaze :: String -> UArray (V2 Int) Char
parseMaze input = array (fst (head grid), fst (last grid)) grid
    where grid = [ ((V2 x y), c) | (y, row) <- zip [0..] (lines input), (x, c) <- zip [0..] row ]

distsToKeys :: UArray (V2 Int) Char -> Set Char -> V2 Int -> [(Char, (Int, V2 Int))]
distsToKeys grid found start = go (S.singleton start) $ neighbors (start, 0)
    where neighbors (V2 x y, depth) =
              [ (coord, depth + 1) | coord <- [V2 (x-1) y, V2 (x+1) y, V2 x (y-1), V2 x (y+1)]
              , let v = grid ! coord
              , v /= '#'
              , not (isUpper v) || toLower v `elem` found
              ]
          go _       [] = []
          go visited (node@(pos, depth) : nodes)
              | S.member pos visited = go visited nodes
              | isLower k && k `notElem` found = (k, (depth, pos)) : go visited' nodes
              | otherwise = go visited' (nodes ++ neighbors node)
              where visited' = S.insert pos visited
                    k = grid ! pos

search :: Char -> UArray (V2 Int) Char -> Int
search key grid = evalState (go keyPoss S.empty) M.empty
    where keyPoss = map fst $ filter ((==key) . snd) $ assocs grid
          go :: [V2 Int] -> Set Char -> State (Map ([V2 Int], Set Char) Int) Int
          go starts found = get >>= maybe (f starts found) pure . M.lookup (starts, found)
          f starts found = do
            ans <- fmap (\x -> if null x then 0 else minimum x)
                   $ mapM (\(i, (ch, (dist, pos))) ->
                               (dist +) <$> go (ix i .~ pos $ starts) (S.insert ch found))
                   $ concat $ zipWith (\i -> map (i,) . distsToKeys grid found) [0..] starts
            modify' $ M.insert (starts, found) ans
            pure ans

part1 :: String -> Int
part1 = search '@' . parseMaze

quadrants :: UArray (V2 Int) Char -> UArray (V2 Int) Char
quadrants maze = maze // zip (range (V2 39 39, V2 41 41)) "@#@###@#@"

part2 :: String -> Int
part2 = search '@' . quadrants . parseMaze
