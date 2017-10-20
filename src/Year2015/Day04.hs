module Year2015.Day04
    ( part1
    , part2
    ) where

import Control.Concurrent (setNumCapabilities)
import Control.Concurrent.Async (async, wait)
import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVar, readTVar, readTVarIO)
import Control.Monad (replicateM)
import Control.Monad.Extra (mapMaybeM)
import Crypto.Hash.MD5 (hash)
import Data.ByteString (ByteString, isPrefixOf, pack)
import qualified Data.ByteString.Char8 as B
import Data.Monoid ((<>))
import Data.List (find)


findHash :: (ByteString -> Bool) -> ByteString -> Int
findHash f seed = head $ filter (f . hash . (seed <>) . B.pack . show) [0..]

part1 :: String -> Int
part1 = findHash f . B.pack
    where zero5s = [pack [0,0,x] | x <- [0..15]]
          f x = any (`isPrefixOf` x) zero5s

process :: Int -> (Int -> Bool) -> TVar Int -> TVar Int -> IO (Maybe Int)
process bufferSize cond current nSolutions = go
    where go = do
            sn <- readTVarIO nSolutions
            if sn > 0 then do
              cn <- atomically $ do
                      x <- readTVar current
                      modifyTVar' current (+bufferSize)
                      return x
              case find cond [cn..cn+bufferSize - 1] of
                Just x -> do
                  atomically $ modifyTVar' nSolutions pred
                  return $ Just x
                Nothing -> go
            else return Nothing

part2 :: ByteString -> IO Int
part2 seed = do
  let bufferSize = 64
      threads = 4
  setNumCapabilities threads
  current <- atomically $ newTVar 0
  nSolutions <- atomically $ newTVar 1
  ps <- replicateM threads $ async $ process bufferSize f current nSolutions
  head <$> mapMaybeM wait ps
    where f = (pack [0,0,0] `isPrefixOf`) . hash . (seed <>) . B.pack . show
