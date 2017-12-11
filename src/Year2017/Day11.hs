{-# LANGUAGE OverloadedStrings #-}

module Year2017.Day11
    ( part1
    , part2
    ) where

import Control.Lens
import Data.Text (Text, splitOn)


type Coord = (Int, Int, Int)

distFromOrigin :: Coord -> Int
distFromOrigin (x, y, z) = maximum $ map abs [x, y, z]

dirFun :: Text -> Coord -> Coord
dirFun "n"  = over _2 succ . over _3 pred
dirFun "ne" = over _1 succ . over _3 pred
dirFun "se" = over _1 succ . over _2 pred
dirFun "s"  = over _3 succ . over _2 pred
dirFun "sw" = over _3 succ . over _1 pred
dirFun "nw" = over _2 succ . over _1 pred

path :: [Text] -> [Coord]
path = scanl (flip ($)) (0, 0, 0) . map dirFun

part1 :: Text -> Int
part1 = distFromOrigin . last . path . splitOn ","

part2 :: Text -> Int
part2 = maximum . map distFromOrigin . path . splitOn ","
