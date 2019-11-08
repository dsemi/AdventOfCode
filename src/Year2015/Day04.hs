module Year2015.Day04
    ( part1
    , part2
    ) where

import Utils

import Control.Parallel.Strategies (parBuffer, runEval, rseq)
import Crypto.Hash.MD5 (hash)
import Data.ByteString (ByteString, isPrefixOf, pack)
import qualified Data.ByteString.Char8 as B
import Data.List (findIndex)
import Data.Monoid ((<>))


part1 :: ByteString -> Int
part1 seed = head $ filter f [0..]
    where zero5s = [pack [0,0,x] | x <- [0..15]]
          f = (\x -> any (`isPrefixOf` x) zero5s) . hash . (seed <>) . B.pack . show

part2 :: ByteString -> IO (Maybe Int)
part2 seed = parallel $ findIndex id $ runEval $ parBuffer 1000 rseq $ map f [0..]
    where f :: Int -> Bool
          f = (pack [0,0,0] `isPrefixOf`) . hash . (seed <>) . B.pack . show
