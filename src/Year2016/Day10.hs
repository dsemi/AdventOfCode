module Year2016.Day10
    ( part1
    , part2
    ) where

import Data.List.Ordered (merge)
import Data.Map.Lazy ((!), Map)
import qualified Data.Map.Lazy as M


data Node = Bot Int | Output Int
            deriving (Eq, Ord)
type Edge = (Node, [Int])
type Graph = Map Node [Int]

num :: Node -> Int
num (Bot n) = n
num (Output n) = n

parse :: (Node -> [Int]) -> [String] -> [Edge]
parse f ["bot",n0,_,_,_,o1,n1,_,_,_,o2,n2] =
    let [a, b] = f $ Bot $ read n0
        minNode = cns o1 $ read n1
        maxNode = cns o2 $ read n2
    in [ (minNode, [a]), (maxNode, [b]) ]
    where cns "bot" = Bot
          cns _     = Output
parse _ ["value",v,_,_,_,n] = [(Bot (read n), [read v])]
parse _ _ = error "Invalid input"

build :: String -> Graph
build xs = m
    where m = M.fromListWith merge . concatMap (parse (m !)) . map words $ lines xs

part1 :: String -> Int
part1 = num . head . M.keys . M.filter (==[17, 61]) . build

part2 :: String -> Int
part2 s = let m = build s
          in product $ concatMap ((m !) . Output) [0..2]
