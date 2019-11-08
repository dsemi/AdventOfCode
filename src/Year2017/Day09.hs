{-# LANGUAGE NamedFieldPuns #-}

module Year2017.Day09
    ( part1
    , part2
    ) where

import Control.Foldl (Fold(..))
import qualified Control.Foldl as L


data Stream = Stream { score :: Int
                     , depth :: Int
                     , inGarbage :: Bool
                     , garbageCount :: Int
                     , ignoreNext :: Bool
                     }

process :: Fold Char Stream
process = Fold f (Stream 0 0 False 0 False) id
    where f stream@(Stream {score, depth, inGarbage, garbageCount, ignoreNext}) x
              | ignoreNext = stream { ignoreNext = False }
              | inGarbage =
                  case x of
                    '!' -> stream { ignoreNext = True }
                    '>' -> stream { inGarbage = False }
                    _   -> stream { garbageCount = garbageCount + 1 }
              | x == '}' = stream { score = score + depth
                                  , depth = depth - 1 }
              | x == '{' = stream { depth = depth + 1 }
              | x == '<' = stream { inGarbage = True }
              | otherwise = stream

part1 :: String -> Int
part1 = score . L.fold process

part2 :: String -> Int
part2 = garbageCount . L.fold process
