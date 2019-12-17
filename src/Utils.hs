{-# LANGUAGE ScopedTypeVariables #-}

module Utils where

import Control.DeepSeq
import Control.Monad.ST
import Data.Either (fromRight)
import Data.Set (Set)
import qualified Data.Set as S
import Data.STRef
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Conc
import Linear.V2
import Text.Megaparsec (Parsec, Stream, many, parse, try, (<|>))
import Text.Megaparsec.Char (anyChar)
import Text.Megaparsec.Char.Lexer (decimal, signed)


findAllInts :: (Integral a) => String -> [a]
findAllInts = findAll (signed (pure ()) decimal)

searchAll :: (Stream s) => Parsec () s a -> Parsec () s a
searchAll p = let parser = try p <|> (anyChar *> parser) in parser

findAll :: (Stream s) => Parsec () s a -> s -> [a]
findAll p = fromRight [] . parse parser ""
    where parser = many $ try $ searchAll p

replace1 :: Text -> Text -> Text -> Text
replace1 needle rep haystack =
    let (a, b) = T.breakOn needle haystack
    in if T.null b then a
       else a `T.append` rep `T.append` T.drop (T.length needle) b

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

bfsOn :: forall a k. (Ord k) => (a -> k) -> a -> (a -> [a]) -> [(Int, a)]
bfsOn f start neighbors = go S.empty [(0, start)]
    where go :: Set k -> [(Int, a)] -> [(Int, a)]
          go _       [] = []
          go visited ((depth, node) : nodes)
              | S.member key visited = go visited nodes
              | otherwise = (depth, node) : go (S.insert key visited)
                            (nodes ++ map (depth+1,) (neighbors node))
              where key = f node

bfs :: (Ord a) => a -> (a -> [a]) -> [(Int, a)]
bfs = bfsOn id

move :: Char -> V2 Int
move '^' = V2 0 (-1)
move 'v' = V2 0 1
move '<' = V2 (-1) 0
move '>' = V2 1 0
move  _  = error "Invalid direction"
