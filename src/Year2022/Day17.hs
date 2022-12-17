{-# LANGUAGE BinaryLiterals #-}

module Year2022.Day17
    ( part1
    , part2
    ) where

import Data.Bits
import qualified Data.HashMap.Strict as M
import qualified Data.Vector.Unboxed as V
import Data.Word

rocks :: [[Word8]]
rocks = [ [0b0011110]
        , [0b0001000, 0b0011100, 0b0001000]
        , [0b0000100, 0b0000100, 0b0011100]
        , [0b0010000, 0b0010000, 0b0010000, 0b0010000]
        , [0b0011000, 0b0011000]]

insertRock :: [Word8] -> [Word8] -> [Word8]
insertRock rock grid = dropWhile (==0) $ go rock grid
    where go [] gs = gs
          go (r:rs) (g:gs) = (r .|. g) : go rs gs
          go _ _ = error "Malformed rock or grid"

solve :: Int -> String -> Int
solve lim input = go M.empty 0 0 0 [0b1111111]
    where dirs = V.fromList input
          go seen addtnlHgt i k befGrid
              | i >= lim = addtnlHgt + length befGrid - 1
              | otherwise = let (k', newGrid) = placeRock k $ rocks !! (i `mod` 5)
                                state = (k' `mod` V.length dirs, i `mod` 5, take 50 grid)
                                seen' = M.insert state (i, length grid - 1) seen
                            in case M.lookup state seen of
                                 Nothing -> go seen' addtnlHgt (i+1) k' newGrid
                                 Just (rockN, height) ->
                                     let hgtDiff = length grid - 1 - height
                                         iDiff = i - rockN
                                         cycles = (lim - i) `div` iDiff
                                     in go seen' (addtnlHgt + cycles*hgtDiff) (i + 1 + cycles*iDiff) k' newGrid
              where grid = replicate (3 + length (rocks !! (i `mod` 5))) 0 ++ befGrid
                    moveLeft rock
                        | any (`testBit` 6) rock = rock
                        | any (/= 0) $ zipWith (.&.) grid rock' = rock
                        | otherwise = rock'
                        where rock' = map (`shiftL` 1) rock
                    moveRight rock
                        | any (`testBit` 0) rock = rock
                        | any (/= 0) $ zipWith (.&.) grid rock' = rock
                        | otherwise = rock'
                        where rock' = map (`shiftR` 1) rock
                    placeRock inpIdx rock
                        | all (== 0) $ zipWith (.&.) rock grid = placeRock (inpIdx+1) $ 0 : rock'
                        | otherwise = (inpIdx, insertRock (tail rock) grid)
                        where c = dirs V.! (inpIdx `mod` V.length dirs)
                              rock' = case c of
                                        '<' -> moveLeft rock
                                        '>' -> moveRight rock
                                        _ -> error "Invalid direction"


part1 :: String -> Int
part1 = solve 2022

part2 :: String -> Int
part2 = solve 1000000000000
