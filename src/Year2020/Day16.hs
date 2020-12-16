{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Year2020.Day16
    ( part1
    , part2
    ) where

import Data.Ix
import Data.List ((\\), isPrefixOf)
import Data.List.Split


type Rule = (String, [(Int, Int)])

parse :: String -> ([Rule], [Int], [[Int]])
parse input = let [rules, yours, others] = splitOn "\n\n" input
              in ( [ (cls, [(a, b), (c, d)]) | line <- lines rules
                   , let [cls, rest] = splitOn ": " line
                   , let [[a, b], [c, d]] = map (map read . splitOn "-") $ splitOn " or " rest ]
                 , map read $ splitOn "," $ last $ lines yours
                 , map (map read . splitOn ",") $ tail $ lines others )

fieldIsValid :: Int -> Rule -> Bool
fieldIsValid field (_, rs) = any (`inRange` field) rs

invalidValues :: [Rule] -> [Int] -> [Int]
invalidValues rules ticket = filter (\t -> not $ any (fieldIsValid t) rules) ticket

part1 :: String -> Int
part1 (parse -> (rules, _, tickets)) = sum $ map (sum . invalidValues rules) tickets

determineFields :: [Rule] -> [[Int]] -> [String]
determineFields rules tix = map (fst . head) $ until (all ((==1) . length)) removeSingles
                            $ foldr (zipWith (filter . fieldIsValid)) (replicate len rules) tix
    where len = length $ head tix
          removeSingles xs = let set = concat $ filter ((==1) . length) xs
                             in map (\x -> if length x == 1 then x else x \\ set) xs

part2 :: String -> Int
part2 (parse -> (rules, yours, tix)) =
    product $ map fst $ filter (("departure" `isPrefixOf`) . snd)
    $ zip yours $ determineFields rules $ filter (null . invalidValues rules) tix
