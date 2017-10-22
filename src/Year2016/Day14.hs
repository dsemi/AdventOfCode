module Year2016.Day14
    ( part1
    , part2
    ) where

import Conduit
import Control.Arrow
import Data.Attoparsec.ByteString.Char8 (Parser, anyChar, char, parseOnly)
import Data.ByteString.Base16 (encode)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.Conduit.List as L
import Data.Either (rights)
import Data.Maybe (listToMaybe)
import Data.Sequence ((|>), Seq)
import qualified Data.Sequence as S
import qualified Crypto.Hash.MD5 as MD5
import Data.Monoid ((<>))


hash :: ByteString -> ByteString
hash = encode . MD5.hash

findAll :: Parser a -> ByteString -> [a]
findAll parser = rights . map (parseOnly parser) . init . B.tails

find3 :: Parser Char
find3 = anyChar >>= char >>= char

find :: (Monad m) => ByteString -> (ByteString -> ByteString) -> Int -> Source m Int
find seed hash n = L.sourceList (map (id &&& hashNum) [0..]) .| go S.empty
    where hashNum = (!! n) . tail . iterate hash . (seed <>) . B.pack . show
          go :: (Monad m) => Seq (Int, Char) -> Conduit (Int, ByteString) m Int
          go pot = do
            Just (i, hashed) <- await
            let threeInARow = listToMaybe $ findAll find3 hashed
                pot' = S.dropWhileL ((>1000) . (i-) . fst) pot
                hasFive p = B.replicate 5 (snd p) `B.isInfixOf` hashed
            case threeInARow of
              Nothing -> go pot'
              Just ch -> let (done, rest) = S.partition hasFive pot'
                         in do
                           yieldMany $ fmap fst done
                           go $ rest |> (i, ch)

part1 :: ByteString -> Int
part1 s = last $ runConduitPure $ find s hash 0 .| L.take 64

part2 :: ByteString -> Int
part2 s = last $ runConduitPure $ find s hash 2016 .| L.take 64
