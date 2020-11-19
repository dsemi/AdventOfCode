module Year2018.Day13
    ( part1
    , part2
    ) where

import DaysTH (UnalteredString(..))

import Control.Lens
import Data.Array
import Data.Set (Set)
import qualified Data.Set as S
import Linear.V2


type Coord = V2 Int
data Turn = L | S | R
data Cart = Cart { pos :: Coord
                 , _dir :: Coord
                 , _ts :: [Turn]
                 }
instance Eq Cart where
    c1 == c2 = pos c1 == pos c2
instance Ord Cart where
    c1 <= c2 = pos c1 <= pos c2

findCarts :: Array Coord Char -> [Cart]
findCarts = map (\(k, v) -> Cart k (f v) $ cycle [L, S, R])
            . filter ((`elem` "<>v^") . snd) . assocs
    where f '^' = V2 (-1) 0
          f '>' = V2 0 1
          f 'v' = V2 1 0
          f '<' = V2 0 (-1)

parseTracks :: String -> (Array Coord Char, Set Cart)
parseTracks input =
    let arr = [ (V2 r c, v) | (r, line) <- zip [0..] $ lines input
              , (c, v) <- zip [0..] line ]
        grid = array (V2 0 0, fst (last arr)) arr
    in (grid , S.fromList $ findCarts grid)

tick :: Array Coord Char -> Set Cart -> Set Cart -> ([Coord], Set Cart)
tick grid movedCarts carts
    | S.null carts = ([], movedCarts)
    | S.member cart movedCarts || S.member cart carts =
        over _1 (pos cart :) $ tick grid (S.delete cart movedCarts) (S.delete cart cartsNoMin)
    | otherwise = tick grid (S.insert cart movedCarts) cartsNoMin
    where (cart, cartsNoMin) = over _1 moveCart $ S.deleteFindMin carts
          turn L (V2 y x) = V2 (-x) y
          turn S (V2 y x) = V2 y x
          turn R (V2 y x) = V2 x (-y)
          moveCart (Cart p d t)
              | v `elem` "-|<>v^" = Cart p' d t
              | v == '\\' = Cart p' (d ^. _yx) t
              | v == '/' = Cart p' (-d ^. _yx) t
              | v == '+' = Cart p' (turn (head t) d) (tail t)
              | otherwise = error "Invalid position"
              where p' = d + p
                    v = grid ! p'

allFinalCartPos :: Array Coord Char -> Set Cart -> [Coord]
allFinalCartPos grid = map (^. _yx) . go
    where go carts
              | S.size carts == 1 = [pos $ S.findMin carts]
              | otherwise = let (crashes, carts') = tick grid S.empty carts
                            in crashes ++ go carts'

part1 :: UnalteredString -> Coord
part1 = head . uncurry allFinalCartPos . parseTracks . unwrap

part2 :: UnalteredString -> Coord
part2 = last . uncurry allFinalCartPos . parseTracks . unwrap
