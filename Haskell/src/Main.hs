{-# LANGUAGE TemplateHaskell #-}

module Main where

import Year2015.Days
import Year2015.Day01
import Year2015.Day02
import Year2015.Day03
import Year2015.Day04
import Year2015.Day05
import Year2015.Day06
import Year2015.Day07
import Year2015.Day08
import Year2015.Day09
import Year2015.Day10
import Year2015.Day11
import Year2015.Day12
import Year2015.Day13
import Year2015.Day14
import Year2015.Day15
import Year2015.Day16
import Year2015.Day17
import Year2015.Day18
import Year2015.Day19
import Year2015.Day20
import Year2015.Day21
import Year2015.Day22
import Year2015.Day23
import Year2015.Day24
import Year2015.Day25

import Control.DeepSeq
import Control.Monad
import Data.List.Split
import Data.String.Utils
import System.Console.ANSI
import System.CPUTime
import System.Environment
import Text.Printf

parseArgs args = let probs = foldr pa [] args
                 in if null probs then [1..25] else probs
    where pa a m
              | all (`elem` '-':['0'..'9']) a = case map read (splitOn "-" a) of
                                                  [s,e] -> [s..e] ++ m
                                                  [n]   -> n : m
                                                  _     -> undefined -- lazy
              | otherwise                     = undefined -- again

findInput :: Int -> IO String
findInput pday = strip <$> readFile ("../inputs/input" ++ show pday ++ ".txt")

$(buildProbs)

colorizeTime :: Double -> String
colorizeTime n = printf "%s%.3f%s" startCode n endCode
    where startCode = setSGRCode [SetColor Foreground Dull c]
          endCode   = setSGRCode [Reset]
          c | n < 0.5   = Green
            | n < 1     = Yellow
            | otherwise = Red

timeFunc :: (NFData a) => IO a -> IO (a, Double)
timeFunc f = do
  start <- getCPUTime
  result <- f
  rnf result `seq` return ()
  end <- getCPUTime
  let elapsedTime = fromIntegral (end - start) / 10^12
  return (result, elapsedTime)

maybeRun :: Int -> IO Double
maybeRun n = maybe notfound run $ lookup n problems
    where notfound = return 0
          str = "Part %d: %28s  Elapsed time %s seconds\n"
          run (p1, p2) = do
            input <- findInput n
            putStrLn $ "Day " ++ show n
            (ans1, elapsedTime1) <- timeFunc $ return $ p1 input
            printf str (1 :: Int) ans1 $ colorizeTime elapsedTime1
            (ans2, elapsedTime2) <- timeFunc $ return $ p2 input
            printf str (2 :: Int) ans2 $ colorizeTime elapsedTime2
            return $ elapsedTime1 + elapsedTime2

main :: IO ()
main = do
  ps <- parseArgs <$> getArgs
  totalTime <- foldM (\acc -> liftM (+acc) . maybeRun) 0 ps
  printf "Total: %49.3f seconds\n" totalTime
