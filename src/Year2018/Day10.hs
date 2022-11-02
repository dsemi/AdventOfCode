module Year2018.Day10
    ( part1
    , part2
    ) where

import Control.Lens (view)
import Data.Maybe
import Linear.V2
import Text.Megaparsec (Parsec, between, optional, parseMaybe)
import Text.Megaparsec.Char (char, space, string)
import Text.Megaparsec.Char.Lexer (decimal, signed)


data Obj = Obj { pos :: V2 Int
               , vel :: V2 Int
               } deriving (Show)

parse :: String -> [Obj]
parse = mapMaybe (parseMaybe parser) . lines
    where parser :: Parsec () String Obj
          parser = Obj <$> (string "position=" *> point <* space)
                   <*> (string "velocity=" *> point)
          point = between (char '<') (char '>') (V2 <$> int <* string ", " <*> int)
          int = optional space >> signed (pure ()) decimal

showObjs :: [Obj] -> String
showObjs objs = '\n' : init (unlines (map (\y -> map (f . (\x -> V2 x y)) [x0..x1]) [y0..y1]))
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
