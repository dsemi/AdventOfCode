{-# LANGUAGE ScopedTypeVariables #-}

module Utils where

import Control.Concurrent (setNumCapabilities)
import Control.Concurrent.Async (async)
import Control.Concurrent.STM
import Control.Concurrent.STM.TMQueue
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Loops
import Data.Conduit
import Data.Conduit.TQueue
import Data.Either (rights)
import Data.List (tails)
import Data.Void (Void)
import Text.Megaparsec (ParseError, Parsec, Token, many, parse, try, (<|>))
import Text.Megaparsec.Char (anyChar)
import Text.Megaparsec.Char.Lexer (decimal, signed)


type Parser = Parsec Void String

findAllInts :: (Integral a) => String -> Either (ParseError (Token String) Void) [a]
findAllInts = ((map fromInteger) <$>) . parse parser ""
    where parser :: Parser [Integer]
          parser = many $ try $ searchAll $ signed (return ()) decimal

searchAll :: Parser a -> Parser a
searchAll p = let parser = try p <|> (anyChar *> parser) in parser

findAll :: Parser a -> String -> [a]
findAll parser = rights . map (parse parser "") . init . tails

multi :: forall a. (a -> Bool) -> (a -> a) -> a -> Source IO a
multi isSolution nextToCheck start = do
  liftIO $ setNumCapabilities threads
  current <- liftIO $ atomically $ newTVar start
  solutions <- liftIO $ atomically $ newTMQueue
  _ <- liftIO $ replicateM threads $ async $ process current solutions
  sourceTMQueue solutions
    where bufferSize = 64
          threads = 4
          process :: TVar a -> TMQueue a -> IO ()
          process current solutions =
              whileM_ (fmap not $ atomically $ isClosedTMQueue solutions) $ do
                buffer <- atomically $ do
                            x <- readTVar current
                            let (xs, next:_) = splitAt bufferSize $ iterate nextToCheck x
                            writeTVar current next
                            return xs
                atomically $ forM_ (filter isSolution buffer) $ writeTMQueue solutions
