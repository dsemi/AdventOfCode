module Year2015.Day08
    ( part1
    , part2
    ) where

import Control.Arrow


part1 :: String -> Int
part1 = sum . map (uncurry (-) . (length &&& length . go)) . lines
    where go = \case
               ('\\':'\\':xs)    -> '\\' : go xs
               ('\\':'"':xs)     -> '"' : go xs
               ('\\':'x':_:_:xs) -> '_' : go xs
               ('"':xs)          -> go xs
               (x:xs)            -> x : go xs
               []                -> []


part2 :: String -> Int
part2 = sum . map ((+2) . sum . map getLen) . lines
    where getLen '\\' = 1
          getLen '"'  = 1
          getLen  _   = 0
