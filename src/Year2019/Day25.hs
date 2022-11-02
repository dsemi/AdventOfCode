module Year2019.Day25
    ( part1
    , part2
    ) where

import Data.Char

import Utils
import Year2019.IntCode


-- Run game with `runIO`
-- Need the following:
--   astrolabe
--   space law space brochure
--   weather machine
--   antenna

-- TODO: Change to a search rather than hardcoding instrs.
instrs :: String
instrs = unlines
         [ "north"
         , "east"
         , "take astrolabe"
         , "south"
         , "take space law space brochure"
         , "north"
         , "west"
         , "north"
         , "north"
         , "north"
         , "north"
         , "take weather machine"
         , "north"
         , "take antenna"
         , "west"
         , "south" ]

part1 :: String -> Int
part1 = last . findAllInts . map chr . runWithInput (map ord instrs) . parse

part2 :: String -> String
part2 = const " "
