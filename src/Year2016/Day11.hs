module Year2016.Day11
    ( part1
    , part2
    ) where

import Utils

import Control.Lens (_1, _2, both, ix, over)
import Data.Graph.AStar
import Data.HashSet (HashSet)
import qualified Data.HashSet as S
import Data.List (delete, lookup, sort)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Text.Megaparsec (choice, some)
import Text.Megaparsec.Char (noneOf, spaceChar, string)


type Pair = (Int, Int) -- First number is floor of Microchip, second is floor of Generator
type Floors = (Int, [Pair])

isDone :: Floors -> Bool
isDone = all (==(3, 3)) . snd

heuristic :: Floors -> Int
heuristic = sum . map (3-) . concatMap (\(a, b) -> [a, b]) . snd

applyToTwo :: (Pair -> Pair) -> [Pair] -> [[Pair]]
applyToTwo f [x, y] = [[f x, f y]]
applyToTwo f (x:xs) = (map (f x :) $ applyToEach f xs) ++ (map (x :) $ applyToTwo f xs)
applyToTwo _ _ = error "Invalid state"

applyToEach :: (Pair -> Pair) -> [Pair] -> [[Pair]]
applyToEach f [x] = [[f x]]
applyToEach f (x:xs) = (f x : xs) : (map (x :) $ applyToEach f xs)
applyToEach _ _ = error "Invalid state"

isValid :: [Pair] -> Bool
isValid = go []
    where go _ [] = True
          go c ((a, b):fs)
              | a == b || a `notElem` (map snd fs ++ c) = go (b:c) fs
              | otherwise = False

neighbors :: Floors -> HashSet Floors
neighbors (e, flrs) = S.fromList [ (e+d, flrs') | d <- dirs
                                 , flrs' <- map sort $ applyToEach (over _1 (f d)) flrs
                                            ++ applyToEach (over _2 (f d)) flrs
                                            ++ applyToEach (over both (f d)) flrs
                                            ++ applyToTwo (over _1 (f d)) flrs
                                            ++ applyToTwo (over _2 (f d)) flrs
                                 , flrs /= flrs'
                                 , isValid flrs'
                                 ]
    where dirs  = filter ((\x -> x >=0 && x < 4) . (+e)) [1, -1]
          f d n = if n == e then n+d else n

parseFloor :: String -> [(String, String)]
parseFloor = findAll $ do
               name  <- spaceChar *> some (noneOf " ")
               type' <- spaceChar *> choice (map string ["microchip", "generator"])
               return $ (head $ splitOn "-" name, type')

makeFloors :: [[(String, String)]] -> Floors
makeFloors = go [] . concatMap (\(f, fs) -> map (\(n, t) -> (n, (f, t))) fs) . zip [0..]
    where go :: [Pair] -> [(String, (Int, String))] -> Floors
          go fls []                 = (0, sort fls)
          go fls ((n, (f, t)) : xs) = let (Just x@(f', _)) = lookup n xs
                                      in if t == "microchip"
                                         then go ((f, f') : fls) $ delete (n, x) xs
                                         else go ((f', f) : fls) $ delete (n, x) xs

part1 :: String -> Int
part1 = length . fromJust . aStar neighbors (\_ -> const 1) heuristic isDone
        . makeFloors . map parseFloor . lines

part2 :: String -> Int
part2 = length . fromJust . aStar neighbors (\_ -> const 1) heuristic isDone . makeFloors
        . over (ix 0) (++ extraItems) . map parseFloor . lines
    where extraItems = [ ("elerium", "generator")
                       , ("elerium", "microchip")
                       , ("dilithium", "generator")
                       , ("dilithium", "microchip")
                       ]
