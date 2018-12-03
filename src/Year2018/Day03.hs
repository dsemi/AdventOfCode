module Year2018.Day03
    ( part1
    , part2
    ) where

import Utils

import Data.Ix
import Data.Array.Unboxed
import Data.Maybe
import Text.Megaparsec (parseMaybe, sepBy)
import Text.Megaparsec.Char (char, string)
import Text.Megaparsec.Char.Lexer (decimal)


type Coord = (Int, Int)
data Claim = Claim { num :: Int
                   , rect :: (Coord, Coord)
                   }

parseClaims :: String -> [Claim]
parseClaims = map (fromJust . parseMaybe parse) . lines
    where parse :: Parser Claim
          parse = do
            n <- char '#' *> decimal
            [x, y] <- string " @ " *> (decimal `sepBy` char ',')
            [w, h] <- string ": " *> (decimal `sepBy` char 'x')
            pure $ Claim n ((x, y), (x+w-1, y+h-1))

coordFreq :: [Claim] -> UArray (Int, Int) Int
coordFreq claims = accumArray (+) 0 ((0, 0), (upperX, upperY)) $ zip ranges $ repeat 1
    where upperX = maximum $ map (fst . snd . rect) claims
          upperY = maximum $ map (snd . snd . rect) claims
          ranges = concatMap (range . rect) claims

part1 :: String -> Int
part1 = length . filter (>1) . elems . coordFreq . parseClaims

findNonOverlappingClaim :: [Claim] -> Claim
findNonOverlappingClaim claims = head $ filter (all ((==1) . (grid !)) . range . rect) claims
    where grid = coordFreq claims

part2 :: String -> Int
part2 = num . findNonOverlappingClaim . parseClaims
