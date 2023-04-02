module Year2022.Day05
    ( part1
    , part2
    ) where

import qualified Data.ByteString.Char8 as B
import Data.Char
import Data.List (foldl')
import Data.List.Split
import Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as M

import Utils hiding (splitOn)

moveStacks :: Bool -> String -> String
moveStacks inOrder input = map head . M.elems . foldl' move (makeStacks crates) $ lines instrs
    where [s, instrs] = splitOn "\n\n" input
          crates = lines s
          num = (length (head crates) + 1) `div` 4
          makeStacks :: [String] -> HashMap Int [Char]
          makeStacks = M.fromList . zip [1..] . foldr (zipWith f) (replicate num []). map (go . tail)
          f Nothing xs = xs
          f (Just x) xs = x:xs
          go [] = []
          go (x:xs) = (if isUpper x then Just x else Nothing) : go (drop 3 xs)
          move stacks instr = M.adjust (toMove ++) b $ M.insert a newStack stacks
              where [n, a, b] = findAllInts $ B.pack instr
                    (tm, newStack) = splitAt n $ stacks ! a
                    toMove = if inOrder then tm else reverse tm

part1 :: String -> String
part1 = moveStacks False

part2 :: String -> String
part2 = moveStacks True
