{-# LANGUAGE NumericUnderscores, OverloadedStrings, QuasiQuotes, ScopedTypeVariables #-}

module Utils where

import Control.DeepSeq
import Control.Monad
import qualified Data.ByteString.Char8 as B
import Conduit
import Data.Either (fromRight)
import Data.Hashable
import qualified Data.HashMap.Strict as M
import qualified Data.HashPSQ as Q
import Data.IORef
import Data.IntSet (IntSet)
import qualified Data.IntSet as I
import Data.List (foldl', tails)
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.String.Interpolate
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GHC.Conc
import Linear.V2
import Math.NumberTheory.Moduli.Chinese
import Network.HTTP.Simple
import System.Clock
import System.Directory
import System.Environment
import System.FilePath.Posix
import System.IO.Unsafe
import Text.Megaparsec
import Text.Megaparsec.Char.Lexer (decimal, signed)

addCookie :: Request -> IO Request
addCookie req = do
  cookie <- B.pack <$> getEnv "AOC_SESSION"
  pure $ addRequestHeader "Cookie" cookie req

downloadFn :: String -> String -> IO ()
downloadFn url outFile = do
  req <- parseRequestThrow url >>= addCookie
  runResourceT $ runConduit $ httpSource req getResponseBody .| sinkFileCautious outFile

submitAnswer :: Int -> Int -> Int -> Text -> IO ()
submitAnswer year day part ans = do
  let body = [("level", B.pack (show part)), ("answer", B.pack (T.unpack ans))]
  req <- setRequestBodyURLEncoded body <$> parseRequestThrow url >>= addCookie
  runResourceT $ runConduit $ httpSource req getResponseBody .| stdoutC
    where url = [i|https://adventofcode.com/#{year}/day/#{day}/answer|]

prevRef :: IORef Integer
prevRef = unsafePerformIO $ getTime Monotonic >>= (\t -> newIORef $ toNanoSecs t `div` 1000 - rateUs)

rateUs :: Integer
rateUs = 5_000_000

getProblemInput :: Int -> Int -> Bool -> IO Text
getProblemInput year day download = do
  exists <- doesFileExist inputFile
  when (not exists && download) $ do
    putStrLn [i|Downloading input for Year #{year} Day #{day}|]
    prev <- readIORef prevRef
    now <- (`div` 1000) . toNanoSecs <$> getTime Monotonic
    let target = prev + rateUs
    when (target > now) $ threadDelay $ fromInteger $ target - now
    getTime Monotonic >>= writeIORef prevRef . (`div` 1000) . toNanoSecs
    createDirectoryIfMissing True $ takeDirectory inputFile
    downloadFn url inputFile
  T.stripEnd <$> TIO.readFile inputFile
    where inputFile = [i|inputs/#{year}/input#{day}.txt|]
          url = [i|https://adventofcode.com/#{year}/day/#{day}/input|]

findAllInts :: (Num a) => String -> [a]
findAllInts = findAll (signed (pure ()) decimal)

searchAll :: (Stream s) => Parsec () s a -> Parsec () s a
searchAll p = let parser = try p <|> (anySingle *> parser) in parser

findAll :: (Stream s) => Parsec () s a -> s -> [a]
findAll p = fromRight [] . parse parser ""
    where parser = many $ try $ searchAll p

replace1 :: Text -> Text -> Text -> Text
replace1 needle rep haystack =
    let (a, b) = T.breakOn needle haystack
    in if T.null b then a
       else a `T.append` rep `T.append` T.drop (T.length needle) b

parallel :: (NFData a) => a -> IO a
parallel x = do
  prev <- getNumCapabilities
  getNumProcessors >>= setNumCapabilities
  x `deepseq` setNumCapabilities prev
  pure x

bfsOn :: forall a k. (Ord k) => (a -> k) -> [a] -> (a -> [a]) -> [(Int, a)]
bfsOn f starts neighbors = go S.empty $ map (0,) starts
    where go :: Set k -> [(Int, a)] -> [(Int, a)]
          go _       [] = []
          go visited ((depth, node) : nodes)
              | S.member key visited = go visited nodes
              | otherwise = (depth, node) : go (S.insert key visited) (nodes ++ map (depth+1,) (neighbors node))
              where key = f node

bfs :: (Ord a) => a -> (a -> [a]) -> [(Int, a)]
bfs x = bfsOn id [x]

bfsMany :: (Ord a) => [a] -> (a -> [a]) -> [(Int, a)]
bfsMany = bfsOn id

bfsOnInt :: forall a. (a -> Int) -> a -> (a -> [a]) -> [(Int, a)]
bfsOnInt f start neighbors = go I.empty [(0, start)]
    where go :: IntSet -> [(Int, a)] -> [(Int, a)]
          go _       [] = []
          go visited ((depth, node) : nodes)
              | I.member key visited = go visited nodes
              | otherwise = (depth, node) :
                            go (I.insert key visited) (nodes ++ map (depth+1,) (neighbors node))
              where key = f node

dijkstra :: forall a. (Hashable a, Ord a) => a -> (a -> [(Int, a)]) -> [(Int, a)]
dijkstra start neighbors = go M.empty $ Q.singleton start 0 start
    where go :: M.HashMap a Int -> Q.HashPSQ a Int a -> [(Int, a)]
          go dists (Q.minView -> Just (_, dist, item, queue))
              | dist <= M.findWithDefault maxBound item dists = (dist, item) : go dists' queue'
              | otherwise = (dist, item) : go dists queue
              where (dists', queue') = foldl' process (M.insert item dist dists, queue) $ neighbors item
                    process (ds, q) (d, st2)
                        | d2 < shrt = (M.insert st2 d2 ds, Q.insert st2 d2 st2 q)
                        | otherwise = (ds, q)
                        where d2 = dist + d
                              shrt = M.findWithDefault (d2+1) st2 ds
          go _ _ = []

move :: Char -> V2 Int
move '^' = V2 0 (-1)
move 'v' = V2 0 1
move '<' = V2 (-1) 0
move '>' = V2 1 0
move  _  = error "Invalid direction"

maybeRead :: (Read a) => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

manyP :: (MonadParsec e s m) => (Token s -> Bool) -> m (Tokens s)
manyP = takeWhileP Nothing

someP :: (MonadParsec e s m) => (Token s -> Bool) -> m (Tokens s)
someP = takeWhile1P Nothing

combinations :: [a] -> Int -> [[a]]
combinations  _ 0 = [[]]
combinations xs n = [ y:ys | y:xs' <- tails xs
                    , ys <- combinations xs' $ n-1 ]

chineseRemainder :: [(Integer, Integer)] -> Maybe Integer
chineseRemainder (x:xs) = uncurry mod <$> foldM chinese x xs
chineseRemainder [] = error "empty list"
