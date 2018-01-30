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
import Data.Sequence ((|>), Seq)
import qualified Data.Sequence as S
import Data.Monoid ((<>))
import Pipes
import qualified Pipes.Prelude as P


hash :: ByteString -> ByteString
hash = encode . MD5.hash

find3 :: Parser Char
find3 = let parser = try threeChar <|> (anyChar >> parser)
        in parser
    where threeChar = anyChar >>= char >>= char

find :: (Monad m) => ByteString -> (ByteString -> ByteString) -> Int -> Producer Int m ()
find seed hash' n = each (map (id &&& hashNum) [0..]) >-> go S.empty
    where hashNum = (!! n) . tail . iterate hash' . (seed <>) . B.pack . show
          go :: (Monad m) => Seq (Int, Char) -> Pipe (Int, ByteString) Int m ()
          go pot = do
            (i, hashed) <- await
            let threeInARow = parseOnly find3 hashed
                pot' = S.dropWhileL ((>1000) . (i-) . fst) pot
                hasFive p = B.replicate 5 (snd p) `B.isInfixOf` hashed
            case threeInARow of
              Left _ -> go pot'
              Right ch -> let (done, rest) = S.partition hasFive pot'
                          in do
                            foldMap yield $ fmap fst done
                            go $ rest |> (i, ch)

part1 :: ByteString -> Int
part1 s = last $ P.toList $ find s hash 0 >-> P.take 64

part2 :: ByteString -> Int
part2 s = last $ P.toList $ find s hash 2016 >-> P.take 64
