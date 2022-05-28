{-# LANGUAGE OverloadedStrings, QuasiQuotes, ScopedTypeVariables #-}

module Utils where

import Control.DeepSeq
import Control.Monad
import Control.RateLimit
import qualified Data.ByteString.Char8 as B
import Data.Either (fromRight)
import Data.IORef
import Data.List (tails)
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.IntSet (IntSet)
import qualified Data.IntSet as I
import Data.String.Interpolate
import Data.Text (Text)
import Data.Time.Units
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GHC.Conc
import Math.NumberTheory.Moduli.Chinese
import Linear.V2
import Pipes
import Pipes.HTTP
import qualified Pipes.ByteString as PB
import System.Directory
import System.Environment
import System.IO
import System.IO.Unsafe
import Text.Megaparsec
import Text.Megaparsec.Char.Lexer (decimal, signed)


downloadFn :: IORef (String -> String -> IO ())
downloadFn = unsafePerformIO $ do
               f <- rateLimitExecution (5 :: Second) go
               newIORef $ curry f
    where go (url, outFile) = do
            req <- parseUrlThrow url >>= addCookie
            manager <- newManager tlsManagerSettings
            withHTTP req manager $ \resp ->
                withFile outFile WriteMode $ \h ->
                    runEffect $ responseBody resp >-> PB.toHandle h
          addCookie req = do
            cookie <- B.pack <$> getEnv "AOC_SESSION"
            pure $ req { requestHeaders = [("Cookie", cookie)] }

getProblemInput :: Int -> Int -> Bool -> IO Text
getProblemInput year day download = do
  exists <- doesFileExist inputFile
  when (not exists && download) $ do
    putStrLn [i|Downloading input for Year #{year} Day #{day}|]
    fn <- readIORef downloadFn
    fn url inputFile
  TIO.readFile inputFile
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

bfsOn :: forall a k. (Ord k) => (a -> k) -> a -> (a -> [a]) -> [(Int, a)]
bfsOn f start neighbors = go S.empty [(0, start)]
    where go :: Set k -> [(Int, a)] -> [(Int, a)]
          go _       [] = []
          go visited ((depth, node) : nodes)
              | S.member key visited = go visited nodes
              | otherwise = (depth, node) :
                            go (S.insert key visited) (nodes ++ map (depth+1,) (neighbors node))
              where key = f node

bfs :: (Ord a) => a -> (a -> [a]) -> [(Int, a)]
bfs = bfsOn id

bfsOnInt :: forall a. (a -> Int) -> a -> (a -> [a]) -> [(Int, a)]
bfsOnInt f start neighbors = go I.empty [(0, start)]
    where go :: IntSet -> [(Int, a)] -> [(Int, a)]
          go _       [] = []
          go visited ((depth, node) : nodes)
              | I.member key visited = go visited nodes
              | otherwise = (depth, node) :
                            go (I.insert key visited) (nodes ++ map (depth+1,) (neighbors node))
              where key = f node

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
