{-# LANGUAGE OverloadedStrings, QuasiQuotes, ScopedTypeVariables #-}

module Utils where

import Control.DeepSeq
import Control.Monad
import qualified Data.ByteString.Char8 as B
import Data.Either (fromRight)
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.String.Interpolate
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Conc
import Linear.V2
import Pipes
import Pipes.HTTP
import qualified Pipes.ByteString as PB
import System.Directory
import System.IO
import Text.Megaparsec
import Text.Megaparsec.Char.Lexer (decimal, signed)


getProblemInput :: Int -> Int -> IO String
getProblemInput year day = do
  exists <- doesFileExist inputFile
  unless exists $ do
    req <- parseUrlThrow url >>= addCookie
    manager <- newManager tlsManagerSettings
    withHTTP req manager $ \resp ->
        withFile inputFile WriteMode $ \h ->
            runEffect $ responseBody resp >-> PB.toHandle h
  readFile inputFile
    where inputFile = [i|inputs/#{year}/input#{day}.txt|]
          url = [i|https://adventofcode.com/#{year}/day/#{day}/input|]
          addCookie req = do
            cookie <- B.readFile "session-cookie"
            pure $ req { requestHeaders = [("Cookie", cookie)] }

findAllInts :: (Integral a) => String -> [a]
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
