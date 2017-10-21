{-# LANGUAGE ScopedTypeVariables #-}

module Utils where

import Control.Concurrent (setNumCapabilities)
import Control.Concurrent.Async (async, wait)
import Control.Concurrent.STM
import Control.Concurrent.STM.TMQueue
import Control.Monad
import Control.Monad.Loops (unfoldM)
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

multi :: forall a. (a -> a) -> (a -> Bool) -> Int -> a -> IO [a]
multi nextToCheck isSolution numSolutions start = do
  setNumCapabilities threads
  current <- atomically $ newTVar start
  solnsLeft <- atomically $ newTVar numSolutions
  solutions <- atomically $ newTMQueue
  ps <- replicateM threads $ async $ process current solnsLeft solutions
  mapM_ wait ps
  atomically $ closeTMQueue solutions
  atomically $ unfoldM (readTMQueue solutions)
    where bufferSize = 64
          threads = 4
          process :: TVar a -> TVar Int -> TMQueue a -> IO ()
          process current solnsLeft solutions = go
              where go = do
                      sn <- readTVarIO solnsLeft
                      when (sn > 0) $ do
                        buffer <- atomically $ do
                                x <- readTVar current
                                let (xs, next:_) = splitAt bufferSize $ iterate nextToCheck x
                                writeTVar current next
                                return xs
                        let solns = filter isSolution buffer
                        when (not $ null solns) $ atomically $ do
                          modifyTVar' solnsLeft (subtract $ length solns)
                          forM_ solns $ writeTMQueue solutions
                        go
