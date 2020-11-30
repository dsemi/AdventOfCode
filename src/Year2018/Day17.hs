{-# LANGUAGE ScopedTypeVariables #-}

module Year2018.Day17
    ( part1
    , part2
    ) where

import Control.Monad
import Control.Monad.Extra
import Control.Monad.Loops
import Control.Monad.ST
import Control.Lens
import Data.Array.ST
import Data.Array.Unboxed
import Data.Maybe
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer


type Coord = (Int, Int)

parseScans :: String -> UArray Coord Char
parseScans s = let clay = concatMap (fromJust . parseMaybe @() parseScan) $ lines s
                   minCoord = (minimum (map fst clay) - 1, minimum (map snd clay))
                   maxCoord = (maximum (map fst clay) + 1, maximum (map snd clay))
               in accumArray (flip const) '.' (minCoord, maxCoord) $ map (,'#') clay
    where parseRange = try (range <$> ((,) <$> decimal <*> (string ".." *> decimal)))
                       <|> ((:[]) <$> decimal)
          parseCoord = (,) <$> anySingle <*> (char '=' *> parseRange)
          parseScan = do
            ranges <- parseCoord `sepBy` string ", "
            pure [ (x, y) | x <- fromJust $ lookup 'x' ranges
                 , y <- fromJust $ lookup 'y' ranges ]

flood :: forall s. STUArray s Coord Char -> ST s (STUArray s Coord Char)
flood grid = go (500, 0) >> pure grid
    where cond :: Coord -> ST s Bool
          cond (x, y) = do
            c <- readArray grid (x, y)
            c' <- readArray grid (x, y+1)
            pure $ c /= '#' && (c' == '#' || c' == '~')
          go :: Coord -> ST s Bool
          go (x, y) = do
            minY <- snd . fst <$> getBounds grid
            maxY <- snd . snd <$> getBounds grid
            if | y < minY -> go (x, y+1)
               | y > maxY -> pure False
               | otherwise -> do
                 c <- readArray grid (x, y)
                 if | c == '|' -> pure False
                    | c == '#' -> pure True
                    | otherwise -> do
                      blocked <- go (x, y+1)
                      if not blocked
                      then do
                        whenM ((== '.') <$> readArray grid (x, y)) $
                              writeArray grid (x, y) '|'
                        pure False
                      else do
                        lefts <- takeWhileM cond $ iterate (over _1 pred) (x, y)
                        rights <- takeWhileM cond $ iterate (over _1 succ) (x, y)
                        nextL <- readArray grid $ over _1 pred (last lefts)
                        nextR <- readArray grid $ over _1 succ (last rights)
                        if nextL == '#' && nextR == '#'
                        then do
                          forM_ (lefts ++ rights) $ \xy -> writeArray grid xy '~'
                          pure True
                        else do
                          forM_ (lefts ++ rights) $ \xy -> writeArray grid xy '|'
                          liftM2 (&&) (go (over _1 pred (last lefts)))
                                     $ go (over _1 succ (last rights))

runWater :: String -> UArray Coord Char
runWater input = runSTUArray $ thaw (parseScans input) >>= flood

part1 :: String -> Int
part1 = length . filter (`elem` "~|") . elems . runWater

part2 :: String -> Int
part2 = length . filter (== '~') . elems . runWater
