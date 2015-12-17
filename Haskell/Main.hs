{-# LANGUAGE TemplateHaskell #-}

module Main where

import Advent.Days
import Advent.Day01
import Advent.Day02
import Advent.Day03
import Advent.Day04
import Advent.Day05
import Advent.Day06
import Advent.Day07
import Advent.Day08
import Advent.Day09
import Advent.Day10
import Advent.Day11
import Advent.Day12
import Advent.Day13
import Advent.Day14
import Advent.Day15
import Advent.Day16
import Advent.Day17
import Advent.Day18
import Advent.Day19
import Advent.Day20
import Advent.Day21
import Advent.Day22
import Advent.Day23
import Advent.Day24
import Advent.Day25
import Advent.Problem

import Control.DeepSeq
import Control.Monad
import Data.List.Split
import Data.String.Utils
import Language.Haskell.TH
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
            (ans1, elapsedTime1) <- timeFunc $ switch p1 input
            printf str (1 :: Int) ans1 $ colorizeTime elapsedTime1
            (ans2, elapsedTime2) <- timeFunc $ switch p2 input
            printf str (2 :: Int) ans2 $ colorizeTime elapsedTime2
            return $ elapsedTime1 + elapsedTime2
          switch p inp = case p of
                       Pure prob -> return . show $ prob inp
                       PureS prob -> return $ prob inp
                       Impure prob -> show <$> prob inp

main :: IO ()
main = do
  ps <- parseArgs <$> getArgs
  totalTime <- foldM (\acc -> liftM (+acc) . maybeRun) 0 ps
  printf "Total: %49.3f seconds\n" totalTime
