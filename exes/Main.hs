{-# LANGUAGE RecordWildCards #-}

module Main where

import Days (problem)
import Utils (getProblemInput, submitAnswer)

import Control.DeepSeq
import Control.Monad.Extra
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.List (maximumBy)
import Data.List.Split
import Data.Ord
import qualified Data.Text as T
import System.Console.ANSI
import System.Clock
import System.Environment
import Text.Printf


data Args = Submit { year :: Int, day :: Int }
          | Run { year :: Int, days :: [Int] }

parseArgs :: [String] -> Args
parseArgs []       = undefined
parseArgs ["submit", y, d] = Submit (read y) (read d)
parseArgs (y:args) = let probs = foldr pa [] args
                     in Run (read y) $ if null probs then [1..25] else probs
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

printOutput :: Int -> ByteString -> Double -> IO ()
printOutput part output t = do
  putStr $ printf "Part %d: " part
  let lns = B.lines output
      len = length lns
  forM_ (zip [0..] lns) $ \(i, ln) -> do
    if i == len - 1
    then if i == 0
         then printf "%54s  Elapsed time %s seconds\n" (T.pack $ B.unpack ln) (colorizeTime t)
         else printf "%-62s  Elapsed time %s seconds\n" (T.pack $ B.unpack ln) (colorizeTime t)
    else B.putStrLn ln

maybeRun :: Int -> Int -> IO (Maybe (Double, ByteString, ByteString))
maybeRun y n = maybe notfound run $ problem y n
    where notfound = do
            putStrLn $ show y ++ " Day " ++ show n ++ " not implemented"
            pure Nothing
          run :: (ByteString -> IO ByteString, ByteString -> IO ByteString)
              -> IO (Maybe (Double, ByteString, ByteString))
          run (p1, p2) = do
            input <- getProblemInput y n True
            putStrLn $ "Day " ++ show n
            (ans1, elapsedTime1) <- timeFunc $ p1 input
            printOutput 1 ans1 elapsedTime1
            (ans2, elapsedTime2) <- timeFunc $ p2 input
            printOutput 2 ans2 elapsedTime2
            putStrLn ""
            pure $ Just (elapsedTime1 + elapsedTime2, ans1, ans2)

main :: IO ()
main = do
  args <- parseArgs <$> getArgs
  case args of
    Submit{..} -> do
     maybeRun year day >>= \case
              Just (_, a1, a2) -> if B.null a2 || a2 == B.pack "0"
                                  then submitAnswer year day 1 a1
                                  else submitAnswer year day 2 a2
              Nothing -> error "Day not implemented"
    Run{..} -> do
      times <- mapMaybeM (\day -> fmap (\(t, _, _) -> (day, t)) <$> maybeRun year day) days
      let (maxDay, maxTime) = maximumBy (comparing snd) times
      let totalTime = sum $ map snd times
      printf "Max: Day %2d %70.3f seconds\n" maxDay maxTime
      printf "Total: %75.3f seconds\n" totalTime
