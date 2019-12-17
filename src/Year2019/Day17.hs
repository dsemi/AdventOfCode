module Year2019.Day17
    ( part1
    , part2
    ) where

import Data.Array.Unboxed
import Data.Char
import Data.List (intercalate)
import Data.List.Split
import Data.Maybe
import qualified Data.IntMap.Strict as M
import Linear.V2

import Year2019.IntCode
import Utils


parseGrid :: [Int] -> UArray (V2 Int) Char
parseGrid input = array (fst (head grid), fst (last grid)) grid
    where grid = [ ((V2 x y), c) | (y, row) <- zip [0..] $ lines $ map chr input
                 , (x, c) <- zip [0..] row ]

isScaffold :: UArray (V2 Int) Char -> V2 Int -> Bool
isScaffold grid p = inRange (bounds grid) p && grid ! p `elem` "#^<>v"

findIntersections :: UArray (V2 Int) Char -> [V2 Int]
findIntersections grid = filter isIntersection $ indices grid
    where neighbors v = map (v+) [V2 1 0, V2 0 1, V2 (-1) 0, V2 0 (-1)]
          isIntersection v = all (isScaffold grid) $ v : neighbors v

part1 :: String -> Int
part1 = sum . map product . findIntersections . parseGrid . runWithInput [] . parse

findPath :: UArray (V2 Int) Char -> [String]
findPath grid = map (intercalate ",") $ chunksOf 2 $ go sPos $ move sDir
    where (sPos, sDir) = head $ filter ((`elem` "^><v") . snd) $ assocs grid
          go pos (V2 x y) = keepMoving "L" (V2 y (-x)) ++ keepMoving "R" (V2 (-y) x)
              where keepMoving c dir = f $ takeWhile (isScaffold grid) $ tail $ iterate (+ dir) pos
                        where f [] = []
                              f (last -> v) = c : show (sum (abs (v - pos))) : go v dir

compress :: [String] -> [[String]]
compress instrs = foldr (\(k, v) i -> intercalate k $ splitOn v i) instrs replMap : map snd replMap
    where replMap = zip [["A"], ["B"], ["C"]] $ fromJust $ go [instrs] (3 :: Int)
          go [] _ = Just []
          go _ 0 = Nothing
          go xs fns = listToMaybe $ do
            i <- [1 .. length (head xs)]
            let candidate = take i $ head xs
            let fragments = filter (not . null) $ concatMap (splitOn candidate) xs
            Just res <- [go fragments (fns - 1)]
            pure $ candidate : res

part2 :: String -> Int
part2 (parse -> mem) = last $ runWithInput (map ord inputs) $ M.insert 0 2 mem
    where inputs = unlines $ (++ ["n"]) $ map (intercalate ",")
                   $ compress $ findPath $ parseGrid $ runWithInput [] mem
