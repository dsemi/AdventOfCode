module Year2016.Day14
    ( part1
    , part2
    ) where

import Control.Applicative
import Control.Arrow
import qualified Crypto.Hash.MD5 as MD5
import Data.Attoparsec.ByteString.Char8 (Parser, anyChar, char, parseOnly, try)
import Data.ByteString.Base16 (encode)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Foldable (toList)
import Data.Sequence ((|>), Seq)
import qualified Data.Sequence as S
import Data.Monoid ((<>))


hash :: ByteString -> ByteString
hash = encode . MD5.hash

find3 :: Parser Char
find3 = let parser = try threeChar <|> (anyChar >> parser)
        in parser
    where threeChar = anyChar >>= char >>= char

find :: ByteString -> (ByteString -> ByteString) -> Int -> [Int]
find seed hash' n = go S.empty $ map (id &&& hashNum) [0..]
    where hashNum = (!! n) . tail . iterate hash' . (seed <>) . B.pack . show
          go :: Seq (Int, Char) -> [(Int, ByteString)] -> [Int]
          go pot ((i, hashed) : xs) =
              let threeInARow = parseOnly find3 hashed
                  pot' = S.dropWhileL ((>1000) . (i-) . fst) pot
                  hasFive p = B.replicate 5 (snd p) `B.isInfixOf` hashed
              in case threeInARow of
                   Left _ -> go pot' xs
                   Right ch -> let (done, rest) = S.partition hasFive pot'
                               in toList (fmap fst done) ++ go (rest |> (i, ch)) xs

part1 :: ByteString -> Int
part1 s = last $ take 64 $ find s hash 0

part2 :: ByteString -> Int
part2 s = last $ take 64 $ find s hash 2016
