module Year2018.Day13
    ( part1
    , part2
    ) where

import DaysTH (UnalteredString(..))

import Control.Arrow
import Control.Lens
import Data.Array
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Tuple


type Coord = (Int, Int)
data Dir = North | East | South | West deriving (Eq, Show)
data Turn = L | S | R deriving (Show)
data Cart = Cart { pos :: Coord
                 , dir :: [Dir]
                 , ts :: [Turn]
                 } deriving (Show)

move :: Dir -> Coord -> Coord
move North = over _1 pred
move East  = over _2 succ
move South = over _1 succ
move West  = over _2 pred

dirs :: [Dir]
dirs = cycle [North, East, South, West]

turns :: [Turn]
turns = cycle [L, S, R]

turn :: Turn -> [Dir] -> [Dir]
turn L = drop 3
turn S = id
turn R = drop 1

findCarts :: Array Coord Char -> [Cart]
findCarts = map (\(k, v) -> Cart k (f v) turns) . filter ((`elem` "<>v^") . snd) . assocs
    where f '^' = dirs
          f '>' = drop 1 dirs
          f 'v' = drop 2 dirs
          f '<' = drop 3 dirs

parseTracks :: String -> (Array Coord Char, Map Coord Cart)
parseTracks input =
    let inputLines = lines input
        rows = length inputLines
        cols = length $ head inputLines
        grid = listArray ((0, 0), (cols-1, rows-1)) $ concat inputLines
        carts = findCarts grid
    in ( grid // map (\c -> (pos c, if head (dir c) `elem` [North, South] then '|' else '-')) carts
       , M.fromList $ map (pos &&& id) carts )

tick :: Array Coord Char -> Map Coord Cart -> Map Coord Cart -> ([Coord], Map Coord Cart)
tick grid movedCarts carts
    | M.null carts = ([], movedCarts)
    | M.member (pos minCart) movedCarts = over _1 (pos minCart :)
                                          $ tick grid (M.delete (pos minCart) movedCarts) cartsNoMin
    | M.member (pos minCart) carts = over _1 (pos minCart :)
                                     $ tick grid movedCarts (M.delete (pos minCart) cartsNoMin)
    | otherwise = tick grid (M.insert (pos minCart) minCart movedCarts) cartsNoMin
    where minCart = moveCart $ snd $ M.findMin carts
          cartsNoMin = M.deleteMin carts
          moveCart (Cart p d t)
              | v `elem` "-|" = Cart p' d t
              | v `elem` "/\\" = Cart p' d' t
              | v == '+' = Cart p' (turn (head t) d) (tail t)
              where p' = move (head d) p
                    v = grid ! p'
                    d' = turn (if (head d) `elem` [North, South] && v == '\\'
                                      || (head d) `elem` [East, West] && v == '/'
                               then L else R) d

findFirstCrash :: Array Coord Char -> Map Coord Cart -> Coord
findFirstCrash grid = go
    where go carts = let (crashes, carts') = tick grid M.empty carts
                     in if null crashes then go carts'
                        else swap $ head crashes

part1 :: UnalteredString -> String
part1 = show . uncurry findFirstCrash . parseTracks . unwrap

findLastCrash :: Array Coord Char -> Map Coord Cart -> Coord
findLastCrash grid = go
    where go carts
              | M.size carts == 1 = swap $ fst $ M.findMin carts
              | otherwise = go $ snd (tick grid M.empty carts)

part2 :: UnalteredString -> String
part2 = show . uncurry findLastCrash . parseTracks . unwrap
