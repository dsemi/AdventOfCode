module Year2017.Day22
    ( part1
    , part2
    ) where

import Control.Monad
import Control.Monad.ST
import Control.Lens
import Data.Array.ST
import Data.STRef


type Coord = (Int, Int)
data NodeState = Cleaned | Weakened | Infected | Flagged deriving (Eq, Enum)
type Grid s = STUArray s Coord Int
data Direction = U | D | L | R

right :: Direction -> Direction
right U = R
right R = D
right D = L
right L = U

left :: Direction -> Direction
left U = L
left L = D
left D = R
left R = U

turn :: NodeState -> Direction -> Direction
turn Cleaned  = left
turn Weakened = id
turn Infected = right
turn Flagged  = right . right

move :: Direction -> Coord -> Coord
move U = over _1 pred
move D = over _1 succ
move L = over _2 pred
move R = over _2 succ

countInfections :: Int -> (NodeState -> NodeState) -> String -> Int
countInfections bursts nextState input =
    runST $ do
      count <- newSTRef 0
      grid <- newArray ((-5000, -5000), (5000, 5000)) $ fromEnum Cleaned
      forM_ parsedGrid $ \(k, v) -> writeArray grid k $ fromEnum v
      go count bursts (0, 0) U grid
    where parsedGrid =
              [ ((r, c), s) | (r, row) <- zip [-12..] $ lines input
              , (c, v) <- zip [-12..] row
              , let s = if v == '#' then Infected else Cleaned ]
          go :: STRef s Int -> Int -> Coord -> Direction -> Grid s -> ST s Int
          go count 0 _ _ _ = readSTRef count
          go count n pos dir grid = do
            val <- toEnum <$> readArray grid pos
            let dir' = turn val dir
                val' = nextState val
            when (val' == Infected) $
                 modifySTRef' count (+1)
            writeArray grid pos $ fromEnum val'
            go count (n-1) (move dir' pos) dir' grid

part1 :: String -> Int
part1 = countInfections 10000 f
    where f Cleaned  = Infected
          f Infected = Cleaned
          f _        = error "Invalid state"

part2 :: String -> Int
part2 = countInfections 10000000 f
    where f Cleaned  = Weakened
          f Weakened = Infected
          f Infected = Flagged
          f Flagged  = Cleaned
