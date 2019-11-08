{-# LANGUAGE ScopedTypeVariables #-}

module Utils where

import Control.DeepSeq
import Control.Monad.ST
import Data.Either (rights)
import Data.Hashable (Hashable)
import Data.HashSet (HashSet)
import qualified Data.HashSet as S
import Data.List (tails)
import Data.STRef
import GHC.Conc
import Text.Megaparsec (ParseError, Parsec, Token, many, parse, try, (<|>))
import Text.Megaparsec.Char (anyChar)
import Text.Megaparsec.Char.Lexer (decimal, signed)


findAllInts :: (Integral a) => String -> Either (ParseError (Token String) ()) [a]
findAllInts = ((map fromInteger) <$>) . parse parser ""
    where parser :: Parsec () String [Integer]
          parser = many $ try $ searchAll $ signed (return ()) decimal

searchAll :: Parsec () String a -> Parsec () String a
searchAll p = let parser = try p <|> (anyChar *> parser) in parser

findAll :: Parsec () String a -> String -> [a]
findAll parser = rights . map (parse parser "") . init . tails

swapSTRef :: STRef s a -> a -> ST s a
swapSTRef ref x = do
  v <- readSTRef ref
  writeSTRef ref x
  pure v

parallel :: (NFData a) => a -> IO a
parallel x = do
  prev <- getNumCapabilities
  getNumProcessors >>= setNumCapabilities
  x `deepseq` setNumCapabilities prev
  pure x

bfs :: forall a. (Eq a, Hashable a) => a -> (a -> [a]) -> [(Int, a)]
bfs start neighbors = go S.empty [(0, start)]
    where go :: HashSet a -> [(Int, a)] -> [(Int, a)]
          go _       [] = []
          go visited ((depth, node) : nodes)
              | S.member node visited = go visited nodes
              | otherwise = (depth, node) : go (S.insert node visited)
                            (nodes ++ map (depth+1,) (neighbors node))
