module Main where

import Days (problem)
import Utils (getProblemInput)

import Control.DeepSeq
import Control.Monad.IO.Class
import Data.List (maximumBy)
import Data.List.Split
import Data.Ord
import System.Console.ANSI
import System.Clock
import System.Environment
import Text.Printf


data Args = Args { year :: Int
                 , probNums :: [Int]
                 }

parseArgs :: [String] -> Args
parseArgs []       = undefined
parseArgs (y:args) = let probs = foldr pa [] args
                     in Args (read y) $ if null probs then [1..25] else probs
    where pa a m
              | all (`elem` '-':['0'..'9']) a = case map read (splitOn "-" a) of
                                                  [s,e] -> [s..e] ++ m
                                                  [n]   -> n : m
                                                  _     -> undefined -- lazy
              | otherwise                     = undefined -- again

colorizeTime :: Double -> String
colorizeTime n = printf "%s%.3f%s" startCode n endCode
    where startCode = setSGRCode [SetColor Foreground Dull c]
          endCode   = setSGRCode [Reset]
          c | n < 0.5   = Green
            | n < 1     = Yellow
            | otherwise = Red

timeFunc :: (NFData a, MonadIO m) => m a -> m (a, Double)
timeFunc f = do
  start <- toNanoSecs <$> liftIO (getTime Monotonic)
  result <- f
  rnf result `seq` pure ()
  end <- toNanoSecs <$> liftIO (getTime Monotonic)
  let elapsedTime = fromIntegral (end - start) / 10^9
  pure (result, elapsedTime)

maybeRun :: Int -> Int -> IO Double
maybeRun y n = maybe notfound run $ problem y n
    where notfound = do
            putStrLn $ show y ++ " Day " ++ show n ++ " not implemented"
            pure 0
          str = "Part %s: %50s  Elapsed time %s seconds\n"
          run (p1, p2) = do
            input <- getProblemInput y n True
            putStrLn $ "Day " ++ show n
            (ans1, elapsedTime1) <- timeFunc $ p1 input
            printf str "1" ans1 $ colorizeTime elapsedTime1
            (ans2, elapsedTime2) <- timeFunc $ p2 input
            printf str "2" ans2 $ colorizeTime elapsedTime2
            putStrLn ""
            pure $ elapsedTime1 + elapsedTime2

main :: IO ()
main = do
  args <- parseArgs <$> getArgs
  times <- mapM (\day -> (day,) <$> maybeRun (year args) day) $ probNums args
  let (maxDay, maxTime) = maximumBy (comparing snd) times
  let totalTime = sum $ map snd times
  printf "Max: Day %2d %66.3f seconds\n" maxDay maxTime
  printf "Total: %71.3f seconds\n" totalTime
