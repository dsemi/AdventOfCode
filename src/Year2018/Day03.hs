module Year2018.Day03
    ( part1
    , part2
    ) where

import Data.Array.Unboxed
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B

import Scanf

type Coord = (Int, Int)
data Claim = Claim { num :: Int
                   , rect :: (Coord, Coord)
                   }

parseClaims :: ByteString -> [Claim]
parseClaims = map parse . B.lines
    where parse line = let (n :+ x :+ y :+ w :+ h :+ ()) = scanf [fmt|#%d @ %d,%d: %dx%d|] line
                       in Claim n ((x, y), (x+w-1, y+h-1))

coordFreq :: [Claim] -> UArray (Int, Int) Int
coordFreq claims = accumArray (+) 0 ((0, 0), (upperX, upperY)) $ zip ranges $ repeat 1
    where upperX = maximum $ map (fst . snd . rect) claims
          upperY = maximum $ map (snd . snd . rect) claims
          ranges = concatMap (range . rect) claims

part1 :: ByteString -> Int
part1 = length . filter (>1) . elems . coordFreq . parseClaims

findNonOverlappingClaim :: [Claim] -> Claim
findNonOverlappingClaim claims = head $ filter (all ((==1) . (grid !)) . range . rect) claims
    where grid = coordFreq claims

part2 :: ByteString -> Int
part2 = num . findNonOverlappingClaim . parseClaims
