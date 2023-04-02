{-# LANGUAGE NegativeLiterals #-}

module Year2020.Day24
    ( part1
    , part2
    ) where

import Control.Applicative
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import FlatParse.Basic
import Linear.V3


dirs :: [V3 Int -> V3 Int]
dirs = [ (+ V3  1 -1  0) -- East
       , (+ V3  0 -1  1) -- Southeast
       , (+ V3 -1  0  1) -- Southwest
       , (+ V3 -1  1  0) -- West
       , (+ V3  0  1 -1) -- Northwest
       , (+ V3  1  0 -1) -- Northeast
       ]

flipTiles :: ByteString -> HashMap (V3 Int) Bool
flipTiles = M.filter id . M.fromListWith xor . map ((,True) . foldr ($) (V3 0 0 0) . reverse)
            . map parse . B.lines
    where pDirs = some $ asum $ zipWith (<$) dirs [ $(string "e"), $(string "se"), $(string "sw")
                                                  , $(string "w"), $(string "nw"), $(string "ne") ]
          parse line = case runParser pDirs line of
                         OK res _ -> res
                         _ -> error "unreachable"

part1 :: ByteString -> Int
part1 = M.size . flipTiles

step :: HashMap (V3 Int) Bool -> HashMap (V3 Int) Bool
step m = M.filter id $ M.mapWithKey black adj
    where adj = M.fromListWith (+) $ map (,1) $ dirs <*> M.keys m
          black k v = if M.lookupDefault False k m then v /= 0 && v <= 2 else v == 2

part2 :: ByteString -> Int
part2 = M.size . (!! 100) . iterate step . flipTiles
