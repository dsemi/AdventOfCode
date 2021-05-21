module Year2019.Day18
    ( part1
    , part2
    ) where

import Control.Lens
import Control.Monad.State
import Data.Array.Unboxed
import Data.Bits
import Data.Char
import Data.Function.Memoize
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.IntSet as S
import Linear.V2


type BitSet = Int

add :: Char -> BitSet -> BitSet
add c = flip setBit $ ord c - ord 'a'

test :: Char -> BitSet -> Bool
test c = flip testBit $ ord c - ord 'a'

deriveMemoizable ''V2

parseMaze :: String -> UArray (V2 Int) Char
parseMaze input = array (fst (head grid), fst (last grid)) grid
    where grid = [ ((V2 x y), c) | (y, row) <- zip [0..] (lines input), (x, c) <- zip [0..] row ]

distsToKeys :: UArray (V2 Int) Char -> BitSet -> V2 Int -> [(Char, (Int, V2 Int))]
distsToKeys grid found start = go (S.singleton (enc start)) $ neighbors (start, 0)
    where cols = succ $ view _x $ snd $ bounds grid
          enc (V2 x y) = x * cols + y
          neighbors (V2 x y, depth) =
              [ (coord, depth + 1) | coord <- [V2 (x-1) y, V2 (x+1) y, V2 x (y-1), V2 x (y+1)]
              , let v = grid ! coord
              , v /= '#'
              , not (isUpper v) || test (toLower v) found
              ]
          go _       [] = []
          go visited (node@(pos, depth) : nodes)
              | S.member ePos visited = go visited nodes
              | isLower k && not (test k found) = (k, (depth, pos)) : go visited' nodes
              | otherwise = go visited' (nodes ++ neighbors node)
              where visited' = S.insert ePos visited
                    k = grid ! pos
                    ePos = enc pos

search :: Char -> UArray (V2 Int) Char -> Int
search key grid = evalState (go keyPoss 0) M.empty
    where keyPoss = map fst $ filter ((==key) . snd) $ assocs grid
          d2k = memoize2 $ distsToKeys grid
          go :: [V2 Int] -> BitSet -> State (Map ([V2 Int], BitSet) Int) Int
          go starts found = get >>= maybe (f starts found) pure . M.lookup (starts, found)
          f starts found = do
            ans <- fmap (\x -> if null x then 0 else minimum x)
                   $ mapM (\(i, (ch, (dist, pos))) ->
                               (dist +) <$> go (ix i .~ pos $ starts) (add ch found))
                   $ concat $ zipWith (\i -> map (i,) . d2k found) [0..] starts
            modify' $ M.insert (starts, found) ans
            pure ans

part1 :: String -> Int
part1 = search '@' . parseMaze

quadrants :: UArray (V2 Int) Char -> UArray (V2 Int) Char
quadrants maze = maze // zip (range (V2 39 39, V2 41 41)) "@#@###@#@"

part2 :: String -> Int
part2 = search '@' . quadrants . parseMaze
