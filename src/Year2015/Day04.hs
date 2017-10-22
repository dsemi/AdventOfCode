module Year2015.Day04
    ( part1
    , part2
    ) where

import Utils

import Crypto.Hash.MD5 (hash)
import Data.ByteString (ByteString, isPrefixOf, pack)
import qualified Data.ByteString.Char8 as B
import Data.Conduit
import Data.Maybe
import Data.Monoid ((<>))


part1 :: ByteString -> Int
part1 seed = head $ filter f [0..]
    where zero5s = [pack [0,0,x] | x <- [0..15]]
          f = (\x -> any (`isPrefixOf` x) zero5s) . hash . (seed <>) . B.pack . show

part2 :: ByteString -> IO Int
part2 seed = fromJust <$> (multi f (+1) 0 $$ await)
    where f = (pack [0,0,0] `isPrefixOf`) . hash . (seed <>) . B.pack . show
