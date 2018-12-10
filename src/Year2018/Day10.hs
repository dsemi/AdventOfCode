{-# LANGUAGE QuasiQuotes #-}

module Year2018.Day10
    ( part1
    , part2
    ) where

import Control.Lens (view)
import Data.Maybe
import Linear.V2
import Text.Regex.PCRE.Heavy


data Obj = Obj { pos :: V2 Int
               , vel :: V2 Int
               } deriving (Show)

parse :: String -> [Obj]
parse = map (f . map read . snd) . scan regex
    where regex = [re|position=<((?:-| )\d+), ((?:-| )\d+)> velocity=<((?:-| )\d+), ((?:-| )\d+)>|]
          f [xPos, yPos, xVel, yVel] = Obj (V2 xPos yPos) (V2 xVel yVel)

showObjs :: [Obj] -> String
showObjs objs = '\n' : unlines (map (\y -> map (f . (\x -> V2 x y)) [x0..x1]) [y0..y1])
    where ((x0, y0), (x1, y1)) = boundingBox objs
          f x = fromMaybe ' ' $ lookup x m
              where m = map ((, '#') . pos) objs

boundingBox :: [Obj] -> ((Int, Int), (Int, Int))
boundingBox objs = ( (minimum $ map (view _x . pos) objs, minimum $ map (view _y . pos) objs)
                   , (maximum $ map (view _x . pos) objs, maximum $ map (view _y . pos) objs) )

findMessage :: [Obj] -> (Int, [Obj])
findMessage = go 0
    where go c objs
              -- 15 seems like a "reasonable" height
              | y1 - y0 <= 15 = (c, objs)
              | otherwise = go (c+1) $ map (\(Obj p v) -> Obj (p + v) v) objs
              where ((_, y0), (_, y1)) = boundingBox objs

part1 :: String -> String
part1 = showObjs . snd . findMessage . parse

part2 :: String -> Int
part2 = fst . findMessage . parse
