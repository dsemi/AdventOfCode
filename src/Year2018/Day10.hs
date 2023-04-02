module Year2018.Day10
    ( part1
    , part2
    ) where

import Control.Lens (view)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Char (isSpace)
import Data.Maybe
import FlatParse.Basic
import Linear.V2

import Ocr
import Utils

data Obj = Obj { pos :: V2 Int
               , vel :: V2 Int
               } deriving (Show)

parse :: ByteString -> [Obj]
parse = map parseLine . B.lines
    where parser = Obj <$> ($(string "position=") *> point <* $(char ' '))
                   <*> ($(string "velocity=") *> point)
          point = $(char '<') *> (V2 <$> int <* $(string ", ") <*> int) <* $(char '>')
          int = skipMany (satisfy isSpace) >> signedInt
          parseLine line = case runParser parser line of
                             OK res _ -> res
                             _ -> error "unreachable"

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

part1 :: ByteString -> String
part1 = parseLetters . showObjs . snd . findMessage . parse

part2 :: ByteString -> Int
part2 = fst . findMessage . parse
