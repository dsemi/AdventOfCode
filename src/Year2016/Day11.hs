module Year2016.Day11
    ( part1
    , part2
    ) where

import Utils

import Control.Comonad.Store (peeks)
import Control.Lens (both, filtered, holesOf)
import Data.Graph.AStar
import Data.HashSet (HashSet)
import qualified Data.HashSet as S
import Data.List (delete, sort)
import Data.List.Split (splitOn)
import Text.Megaparsec (choice, noneOf, some)
import Text.Megaparsec.Char (spaceChar, string)


type Pair = (Int, Int) -- First number is floor of Microchip, second is floor of Generator
type Floors = (Int, [Pair])

isDone :: Floors -> Bool
isDone = all (==(3, 3)) . snd

heuristic :: Floors -> Int
heuristic = sum . map (3-) . concatMap (\(a, b) -> [a, b]) . snd

allMoves :: Int -> Int -> [Pair] -> [[Pair]]
allMoves e e' flrs = let oneMoved = f flrs
                     in filter isValid $ oneMoved ++ concatMap f oneMoved
    where f = fmap (peeks (const e')) . holesOf (traverse . both . filtered (==e))

isValid :: [Pair] -> Bool
isValid = go []
    where go _ [] = True
          go c ((a, b):fs)
              | a == b || a `notElem` (map snd fs ++ c) = go (b:c) fs
              | otherwise = False

neighbors :: Floors -> HashSet Floors
neighbors (e, flrs) =
    S.fromList $ do
      e' <- filter (\x -> x >=0 && x < 4) [e+1, e-1]
      flrs' <- map sort $ allMoves e e' flrs
      pure (e', flrs')

parseFloor :: Int -> String -> [(String, (Int, String))]
parseFloor f =
    findAll $ do
      name  <- spaceChar *> some (noneOf " ")
      type' <- spaceChar *> choice (map string ["microchip", "generator"])
      pure $ (head $ splitOn "-" name, (f, type'))

makeFloors :: String -> Floors
makeFloors = go [] . concatMap (uncurry parseFloor) . zip [0..] . lines
    where go :: [Pair] -> [(String, (Int, String))] -> Floors
          go fls []                 = (0, sort fls)
          go fls ((n, (f, t)) : xs) = let (Just x@(f', _)) = lookup n xs
                                      in if t == "microchip"
                                         then go ((f, f') : fls) $ delete (n, x) xs
                                         else go ((f', f) : fls) $ delete (n, x) xs

part1 :: String -> Maybe Int
part1 = fmap length . aStar neighbors (\_ _ -> 1) heuristic isDone . makeFloors

part2 :: String -> Maybe Int
part2 = fmap length . aStar neighbors (\_ _ -> 1) heuristic isDone . addExtras . makeFloors
    where addExtras (e, flrs) = ( e
                                , (0, 0) : -- elerium
                                  (0, 0) : -- dilithium
                                  flrs)
