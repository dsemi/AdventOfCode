module Year2019.Day18
    ( part1
    , part2
    ) where

import Control.Lens
import Data.Array.Unboxed
import Data.Bits
import Data.Char
import Data.Function.Memoize
import qualified Data.HashMap.Strict as M
import qualified Data.IntSet as S
import qualified Data.PQueue.Min as PQ
import Data.Word
import Linear.V2


deriveMemoizable ''V2

parseMaze :: String -> UArray (V2 Int) Char
parseMaze input = array (fst (head grid), fst (last grid)) grid
    where grid = [ ((V2 x y), c) | (y, row) <- zip [0..] (lines input), (x, c) <- zip [0..] row ]

c2i :: Char -> Word32
c2i c = bit $ ord c - ord 'a'

enc :: UArray (V2 Int) Char -> V2 Int -> Int
enc grid (V2 x y) = x * cols + y
    where cols = succ $ view _x $ snd $ bounds grid

data Edge = Edge { _dest :: V2 Int
                 , _doors :: Word32
                 , _keys :: Word32
                 , _len :: Int
                 }
makeLenses ''Edge

neighbors :: UArray (V2 Int) Char -> V2 Int -> V2 Int -> [V2 Int]
neighbors grid src (V2 x y) = [ p | p <- [V2 (x-1) y, V2 (x+1) y, V2 x (y-1), V2 x (y+1)]
                               , p /= src
                               , grid ! p /= '#']

availableMoves :: UArray (V2 Int) Char -> V2 Int -> [Edge]
availableMoves grid src = go S.empty [Edge src 0 0 0]
    where go _ [] = []
          go visited (edge : rest)
              | S.member k visited = go visited rest
              | otherwise = go2 (edge & len +~ 1) (S.insert k visited) rest
                            $ neighbors grid src (edge ^. dest)
              where k = enc grid (edge ^. dest)
          go2 _ visited rest [] = go visited rest
          go2 edge visited rest (p:ps)
              | isLower ch = let edge2 = edge' & keys %~ (.|. c2i ch)
                             in edge2 : go2 edge visited (rest ++ [edge2]) ps
              | isUpper ch = go2 edge visited (rest ++ [edge' & doors %~ (.|. c2i (toLower ch))]) ps
              | otherwise = go2 edge visited (rest ++ [edge']) ps
              where edge' = edge & dest .~ p
                    ch = grid ! p

data State = State { poss :: [V2 Int]
                   , found :: Word32
                   , depth :: Int
                   } deriving (Eq)

instance Ord State where
    compare a b  = compare (depth a) (depth b)

search :: Char -> UArray (V2 Int) Char -> Maybe Int
search key grid = PQ.minView (PQ.singleton $ State keyPoss 0 0) >>= go M.empty
    where keyPoss = map fst $ filter ((==key) . snd) $ assocs grid
          ks = foldr1 (.|.) $ map c2i $ filter isLower $ elems grid
          availMoves = memoize $ availableMoves grid
          go dists (state, pq)
              | found state == ks = Just $ depth state
              | depth state <= M.lookupDefault maxBound (poss state, found state) dists =
                  let m = M.insert (poss state, found state) (depth state) dists
                      sts = [ State poss2 found2 depth2 | (i, p) <- zip [0..] $ poss state
                            , edge <- availMoves p
                            , found state .&. edge ^. doors == edge ^. doors
                            , found state .&. edge ^. keys /= edge ^. keys
                            , let poss2 = ix i .~ (edge ^. dest) $ poss state
                            , let found2 = found state .|. edge ^. keys
                            , let depth2 = depth state + edge ^. len
                            , depth2 < M.lookupDefault maxBound (poss2, found2) m
                            ]
                      (m', q') = foldr (\st (d, q) -> ( M.insert (poss st, found st) (depth st) d
                                                      , PQ.insert st q))
                                 (m, pq) sts
                  in PQ.minView q' >>= go m'
              | otherwise = PQ.minView pq >>= go dists

part1 :: String -> Maybe Int
part1 = search '@' . parseMaze

quadrants :: UArray (V2 Int) Char -> UArray (V2 Int) Char
quadrants maze = maze // zip (range (V2 39 39, V2 41 41)) "@#@###@#@"

part2 :: String -> Maybe Int
part2 = search '@' . quadrants . parseMaze
